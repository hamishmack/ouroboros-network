{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


-- | This subsystem manages the discovery and selection of /upstream/ peers.
--
module Ouroboros.Network.PeerSelection (
    -- * Design overview
    -- $overview

    -- * Peer selection governor
    -- $peer-selection-governor

    PeerSelectionPolicy(..),
    PeerSelectionTargets(..),
    PeerSelectionActions(..),
--    peerSelectionGovernor,

    -- * Peer churn governor
    -- $peer-churn-governor
--    peerChurnGovernor,

    -- Everything, to avoid unused code warnings during dev
    module Ouroboros.Network.PeerSelection,
) where

import           Data.Void (Void)
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.PriorityQueue.FingerTree as PQueue
import           Data.PriorityQueue.FingerTree (PQueue)

import           Control.Applicative (Alternative(empty, (<|>)))
import           Control.Monad (when, unless)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer(..), traceWith, showTracing, stdoutTracer)
import           Control.Exception (assert, SomeException, SomeAsyncException)

-- for DNS provider
import           Data.Word (Word32)
import           Data.IP (IPv4)
import qualified Network.DNS as DNS

-- for examples:
import           Control.Monad.IOSim
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTimer





{- $overview

We have a number of requirements for constructing our connectivity graphs:

 * We must do it in a decentralised way, using only local information;
 * It should avoid and recover from accidental or deliberate partitions or
   eclipse attacks;
 * The graph should give us good performance for block diffusion. This means
   we need the combination of low hop counts, and minimising the hop lengths.
   We want one slot leader to be able to send to the next within the deadline
   in at least 95% of cases.

[\"Small world" graph theory](https://press.princeton.edu/books/paperback/9780691117041/small-worlds)
tells us that we can use random graph construction to make graphs with a low
characteristic path length (i.e. hop count). We can build random graphs with
random gossip techniques. This deals with our requirement for decentralisation
and our goal of low hop counts.

The remaining significant issues are:

 * the goal of short hop lengths, and
 * avoiding and recovering from partitions and eclipse attacks.

Our design is to augment random gossip with two /governors/ (control loops) to
address these two issues. The design is relatively simple, and has the virtue
that the policy for the governors can be adjusted with relatively few
compatibility impacts. This should enable the policy to be optimised based on
real-world feedback, and feedback from simulations of scale or scenarios that
are hard (or undesirable) to test in a real deployment.

Each node maintains three sets of known peer nodes:

 [cold peers]: are peers that are known of but where there is no established
               network connection;

 [warm peers]: are peers where a bearer connection is established but it is used
               only for network measurements and is not used for any application
               level consensus protocols;

 [hot peers]: are peers where the bearer connection is actively used for the
              application level consensus protocols.

Limited information is maintained for these peers, based on previous direct
interactions. For cold nodes this will often be absent as there may have been
no previous direct interactions. This information is comparable with
\"reputation\" in other systems, but it should be emphasised that it is purely
local and not shared with any other node. It is not shared because it is not
necessary and because establishing trust in such information is difficult and
would add additional complexity. The information about peers is kept
persistently across node restarts, but it is always safe to re-bootstrap – as
new nodes must do.

For an individual node to join the network, the bootstrapping phase starts by
contacting root nodes and requesting sets of other peers. Newly discovered
peers are added to the cold peer set. It proceeds iteratively by randomly
selecting other peers to contact to request more known peers. This gossip
process is controlled by a governor that has a target to find and maintain a
certain number of cold peers. Bootstrapping is not a special mode, rather it is
just a phase for the governor following starting with a cold peers set
consisting only of the root nodes. This gossiping aspect is closely analogous
to the first stage of Kademlia, but with random selection rather than selection
directed towards finding peers in an artificial metric space.

The root nodes used in the bootstrapping phase are the stakepool relays
published in the blockchain as part of the stakepool registration process.
See the [Shelley delegation design specification, Sections 3.4.4 and 4.2](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec).
As with Bitcoin, a recent snapshot of this root set must be distributed with
the software.

The peer selection governor engages in the following activities:

 * the random gossip used to discover more cold peers;
 * promotion of cold peers to be warm peers;
 * demotion of warm peers to cold peers;
 * promotion of warm peers to hot peers; and
 * demotion of hot peers to warm peers.

The peer selection governor has these goals to establish and maintain:

 * a target number of cold peers (e.g. 1000)
 * a target number of hot peers (e.g. order of 2–20)
 * a target number of warm peers (e.g. order of 10–50)
 * a set of warm peers that are sufficiently diverse in terms of hop distance
 * a target churn frequency for hot\/warm changes
 * a target churn frequency for warm\/cold changes
 * a target churn frequency for cold\/unknown changes

The target churn values are adjusted by the /peer churn governor/, which we
will discuss below.

Local static configuration can also be used to specify that certain known nodes
should be selected as hot or warm peers. This allows for fixed relationships
between nodes controlled by a single organisation, such as a stake pool with
several relays. It also enables private peering relationships between stake
pool operators and other likely deployment scenarios.

Using 5–20 hot peers is not as expensive as it might sound. Keep in mind that
only block headers are sent for each peer. The block body is typically only
requested once. It is also worth noting that the block body will tend to follow
the shortest paths through the connectivity graph formed by the hot peer links.
This is because nodes will typically request the block body from the first node
that sends the block header.

While the purpose of cold and hot peers is clear, the purpose of warm peers
requires further explanation. The primary purpose is to address the challenge
of avoiding too many long hops in the graph. The random gossip is oblivious to
hop distance. By actually connecting to a selection of peers and measuring the
round trip delays we can start to establish which peers are near or far. The
policy for selecting which warm peers to promote to hot peers will take into
account this network hop distance. The purpose of a degree of churn between
cold and warm peers is, in part, to discover the network distance for more
peers and enable further optimisation or adjust to changing conditions. The
purpose of a degree of churn between warm and hot peers is to allow potentially
better warm peers to take over from existing hot peers.

The purpose in maintaining a diversity in hop distances is to assist in
recovery from network events that may disrupt established short paths, such as
internet routing changes, partial loss of connectivity, or accidental formation
of cliques. For example, when a physical infrastructure failure causes the
short paths to a clique of nodes to be lost, if some or all of the nodes in
that clique maintain other longer distance warm links then they can quickly
promote them to hot links and recover. The time to promote from warm to hot
need be no more than one network round trip.

Overall, this approach follows a common pattern for probabilistic search or
optimisation that uses a balance of local optimisation with some elements of
higher order disruption to avoid becoming trapped in some poor local optimum.

The local peer reputation information is also updated when peer connections
fail. The implementation classifies the exceptions that cause connections to
fail into three classes:

 * internal node exceptions e.g. local disk corruption;
 * network failures e.g. dropped TCP connections; and
 * adversarial behaviour, e.g. a protocol violation detected by the
   typed-protocols layer or by the consensus layer.

In the case of adversarial behaviour the peer can be immediately demoted out of
the hot, warm and cold sets. We choose not to maintain negative peer
information for extended periods of time; to bound resources and due to the
simplicity of Sybil attacks.

The peer churn governor deals with the problem of partition and eclipse –
whether malicious or accidental. It adjusts the behaviour of the peer
selection governor over longer time scales. The outer peer churn governor's
actions are:

 * to adjust the target churn frequencies of the peer selection governor for
   promotion\/demotion between the cold\/warm\/hot states
 * partial or total re-bootstrapping under certain circumstances

The peer churn governor monitors the chain growth quality, comparing it with
the stake distribution. The probability of being in a disconnected clique or
being eclipsed is calculated. As this rises the governor increases the target
frequencies for the churn between the hot, warm, cold, and unknown states. In
the worst case it can re-bootstrap the peer discovery entirely by resetting
the set of known peers.
-}

{-

TODO: need to think about managing established connections with upstream/downstream peers in a more symmetric way.

Can we separate that connection management from policy of upstream/downstream selection?

Upstream peers are ones where we choose to talk to them, and we follow their
chain and submit transactions to them. There is a separate subsystem to manage
/downstream/ peers that initiate connections to us.

There is a distinction between which peer chooses to talk to which, and which
peer actually initiates the TCP connection. This is due to the fact that we
reuse TCP connections to run mini-protocols in both directions. So we can
choose to talk to another peer and find that they already initiated a TCP
connection to us, and so we reuse that. For example we can have cases like this:

 1. They initiate the connection to have our node as one of their upstream peers
 2. We decide to reuse the connection to have them as one of our upstream peers
 3. They decide to stop using us as an upstream peer

This is now more or less equivalent to our node having initiated the connection
in the first place because we chose to have them as an upstream peer.


-}


{- $peer-selection-governor

The 'peerSelectionGovernor' manages the discovery and selection of /upstream/
peers.

We classify (potential or actual) upstream peers in three nested categories:

@
                                                      ▲
                                               forget │
  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━┿━━━━━━━━━━━━┓
  ┃                                                     │ discover   ┃
  ┃  Known peers: the set of all known peers.           ▼            ┃
  ┃  Consists of cold, warm and hot peers.                           ┃
  ┃  Expect ~1000                              demote ▲              ┃
  ┃                                            to cold│              ┃
  ┃ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━┿━━━━━━━━━━┓ ┃
  ┃ ┃                                                   │ promote  ┃ ┃
  ┃ ┃  Established peers: with established bearer.      ▼ to warm  ┃ ┃
  ┃ ┃  Consists of warm and hot peers.                             ┃ ┃
  ┃ ┃  Expect ~10-50                           demote ▲            ┃ ┃
  ┃ ┃                                          to warm│            ┃ ┃
  ┃ ┃ ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┿━┿━━━━━━━━┓ ┃ ┃
  ┃ ┃ ┃                                                 │ promote┃ ┃ ┃
  ┃ ┃ ┃  Active peers: running consensus protocols.     ▼ to hot ┃ ┃ ┃
  ┃ ┃ ┃  Consists of hot peers.                                  ┃ ┃ ┃
  ┃ ┃ ┃  Expect ~2-20                                            ┃ ┃ ┃
  ┃ ┃ ┃                                                          ┃ ┃ ┃
  ┃ ┃ ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ ┃ ┃
  ┃ ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ ┃
  ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
@

We define the terms /known/, /established/ and /active/ to be nested sets.
We define the terms /cold/, /warm/ and /hot/ to be disjoint sets. Both
collections of terms are useful. For example there is information wish to
track for all known peers, irrespective of whether they are cold, warm or hot.

So we have six transitions to consider:

 * discover a cold peer
 * promote a cold peer to warm
 * promote a warm peer to hot
 * demote a hot peer to warm
 * demote a warm peer to cold
 * forget a cold peer

We want a design that separates the policy from the mechanism. We must
consider what kinds of policy we might like to express and make sure that
information that the policy needs can be made available.

We will consider each case.

== Discovering cold peers

There are two main mechanisms by which we discover cold peers:

 * Externally supplied peer root set
 * Peer gossip

=== Externally supplied peer root set

There are a few potential sources for root sets:

 * Simulation environment
 * IP address lists from static or dynamic configuration
 * DNS names from static or dynamic configuration
 * IP addresses or DNS names for stake pools registered in the blockchain

Note that none of these sources are fully static except for IP addresses from
static configuration. DNS name to IP address mappings are potentially dynamic.
DNS names can refer to both IPv4 and IPv6 addresses, and to pools of addresses.

In some cases we wish to advertise these root peers to others, and sometimes
we want to keep them private. In particular the deployment for stake pools may
involve keeping the stake pool node itself private, and only advertising
relays.

For an externally supplied peer root set, we divide the problem in two with an
interface where a root set provider is responsible for managing a time-varying
set of addresses, and the peer selection governor observes the time-varying
value. This allows multiple implementations of the root set provider, which
deal with the various sources.

=== Peer gossip

We can ask peers to give us a sample of their set of known peers.

For cold peers we can establish a one-shot connection to ask. For warm peers
we can also ask. We should not ask from the same peer too often. Peers are
expected to return the same set of answers over quite long periods of time.
(This helps peers to distinguish abusive behaviour and reduce the speed with
which the whole network can be mapped.)

So factors we might wish to base our decision on:

 * if we are below the target number of known peers
 * if there are any known peers we have not asked (or attempted to ask)
 * how long since we last asked (so we do not ask too frequently)
 * the known distance of the peer from the root set

This last factor is interesting. Consider what happens if we do a bootstrap
from one root peer. We'll ask it for some more peers and it will give us a
selection. Suppose we pick one of these to get more peers from and it gives us
a similar number of replies. If we now pick the next one randomly from our
combined set we'll have a roughly 50:50 chance of picking from either set.
This approach could quickly lead us into a mostly-depth first exploration of
the graph. But we probably want a more balanced approach between breadth first
and depth first. The traditional ways to do a breadth first or depth first is
to keep a queue or a stack of nodes that have not yet been asked.

Here's another danger: suppose we ask several nodes in parallel but suppose
one gets back to us quicker than all the others. If we are too quick to choose
again then we are giving an advantage to fast peers, and adversaries could
dedicate resources to exploit this to their advantage to get nodes to pick up
more peers from the set supplied by the adversary.

So this suggests that we should not give undue advantage to peers that respond
very quickly, and we should go mostly breadth first, but with a degree of
randomisation.


== Promoting a cold peer to warm

Promoting a cold peer to warm involves establishing a bearer connection. This
is initiated asynchronously and it is either successful or fails after a
timeout.

Once established, we track the connection for the established peer. The
established connection is used later to promote to hot, or to demote back to
cold. It is also used to be notified if the connection fails for any reason.

== Promoting a warm peer to hot

Promoting a warm peer to hot involves sending messages on the established
bearer to switch mode from the network protocol used with warm peers, to the
full set of consensus protocols used for hot peers.

== Demoting a hot peer to warm

If we have more hot peers than our target number (or target range) then we
want to pick one to demote. One policy is to pick randomly. It is likely to be
better to to pick the peer that is in some sense least useful.

One plausible measure of a peer being least useful is based on the following:
for blocks we adopt into our chain, look at which peer(s) received that header
first. A peer that is never first (or very rarely) is one that is likely to be
downstream from us and hence not useful as a choice of upstream peer. A peer
that is normally behind all others, but sometimes (even rarely) is first is
still useful, since it shows it's an upstream connection to some part of the
network where there are active block producers. Consider the case of a relay
in Europe with one connection to Australia: sometimes blocks will be produced
in Australia and so that connection may be first in those cases.

Tracking the necessary information for this policy would require a separate
component that observes the current chain and the peer candidate chains. Using
this information would need access to that shared state. So we should conclude
that the policy should not be pure as it may need access to such changing state.

== Demoting a warm peer to cold


== Forgetting cold peers

We will always forget known peers when the connection is terminated due to
detected adversarial behaviour. The remaining policy decision is which peers
to forget when we have more than our target number of known peers. We will
only select from the known peers that are cold. Warm or hot known peers have
to first be demoted to cold before we consider them to be forgotten.

We want to pick the least useful cold peers to forget. Factors we may wish to
base our decision on include:

 * Number of unsuccessful connection attempts since last successful connection
 * Pseudo-random selection: some degree of randomness can help mitigate
   accidental systematic correlations or some degree of adversarial behaviour.

-}


data PeerSelectionPolicy peer m = PeerSelectionPolicy {

       --TODO: where do we decide how many to pick in one go?
       policyPickKnownPeersForGossip :: Maybe (Map peer () -> Int -> m [peer]),

       policyPickColdPeersToPromote  :: Map peer () -> Int -> m [peer],
       policyPickWarmPeersToPromote  :: Map peer () -> Int -> m [peer],
       policyPickHotPeersToDemote    :: Map peer () -> Int -> m [peer],
       policyPickWarmPeersToDemote   :: Map peer () -> Int -> m [peer],
       policyPickColdPeersToForget   :: Map peer () -> Int -> m [peer]

     }

-- | Adjustable targets for the peer selection mechanism.
--
-- These are used by the peer selection governor as targets. They are used by
-- the peer churn governor loop as knobs to adjust, to influence the peer
-- selection governor.
--
data PeerSelectionTargets = PeerSelectionTargets {

       targetNumberOfKnownPeers       :: !Int,
       targetNumberOfEstablishedPeers :: !Int,
       targetNumberOfActivePeers      :: !Int,

       -- Expressed as intervals rather than frequencies
       targetChurnIntervalKnownPeers       :: !DiffTime,
       targetChurnIntervalEstablishedPeers :: !DiffTime,
       targetChurnIntervalActivePeers      :: !DiffTime
     }

nullPeerSelectionTargets :: PeerSelectionTargets
nullPeerSelectionTargets =
    PeerSelectionTargets {
       targetNumberOfKnownPeers       = 0,
       targetNumberOfEstablishedPeers = 0,
       targetNumberOfActivePeers      = 0,
       targetChurnIntervalKnownPeers       = 0,
       targetChurnIntervalEstablishedPeers = 0,
       targetChurnIntervalActivePeers      = 0
    }

sanePeerSelectionTargets :: PeerSelectionTargets -> Bool
sanePeerSelectionTargets PeerSelectionTargets{..} =
                                 0 <= targetNumberOfActivePeers
 && targetNumberOfActivePeers      <= targetNumberOfEstablishedPeers
 && targetNumberOfEstablishedPeers <= targetNumberOfKnownPeers

 && targetNumberOfActivePeers      <= 100
 && targetNumberOfEstablishedPeers <= 1000
 && targetNumberOfKnownPeers       <= 10000


-- | Actions performed by the peer selection governor.
--
-- These being pluggable allows:
--
-- * choice of known peer root sets
-- * running both in simulation and for real
--
data PeerSelectionActions peeraddr m = PeerSelectionActions {

       -- | Read the current set of root peers.
       --
       -- In general this is expected to be updated asynchronously by some
       -- other thread. It is intended to cover use cases including:
       --
       -- * statically configured peers
       -- * federated relays from a DNS pool
       -- * stake pool relays published in the blockchain
       --
       readRootPeerSet :: STM m (RootPeerSet peeraddr),

       readPeerSelectionTargets :: STM m PeerSelectionTargets,

       -- | The action to contact a known peer and request a sample of its
       -- known peers.
       --
       -- This is synchronous, but it should expect to be interrupted by a
       -- timeout asynchronous exception.
       --
       -- TODO: decide how to handle failures: throw exceptions or return?
       --
       requestPeerGossip :: peeraddr -> m (Time, Maybe [peeraddr])

     }


-- | The internal state used by the 'peerSelectionGovernor'.
--
data PeerSelectionState peeraddr = PeerSelectionState {

       targets              :: !PeerSelectionTargets,

       -- | The current set of root peers.
       --
       rootPeers            :: !(Map peeraddr RootPeerInfo),

       -- |
       --
       knownPeers           :: !(Map peeraddr KnownPeerInfo),

       -- |
       --
       establishedPeers     :: !(Map peeraddr ()),

       -- |
       --
       activePeers          :: !(Map peeraddr ()),

       -- | Keep track of the next time we are allowed to send each peer a
       -- gossip request. This is used to make sure we don't send requests too
       -- frequently. This contains all the peers in the 'knownPeers' map.
       --
       knownPeersNextGossip :: !(PQueue Time peeraddr)
     }

emptyPeerSelectionState :: PeerSelectionState peeraddr
emptyPeerSelectionState =
    PeerSelectionState {
      targets              = nullPeerSelectionTargets,
      rootPeers            = Map.empty,
      knownPeers           = Map.empty,
      establishedPeers     = Map.empty,
      activePeers          = Map.empty,
      knownPeersNextGossip = PQueue.empty
    }

invariantPeerSelectionState :: Ord peeraddr
                            => PeerSelectionState peeraddr -> Bool
invariantPeerSelectionState PeerSelectionState{..} =
    Map.isSubmapOfBy (\_ _ -> True) rootPeers knownPeers
 && Map.isSubmapOfBy (\_ _ -> True) activePeers establishedPeers
 && Map.isSubmapOfBy (\_ _ -> True) establishedPeers knownPeers
 && (Set.fromList (Foldable.toList knownPeersNextGossip)
     == Map.keysSet knownPeers)


type RootPeerSet peeraddr = Map peeraddr RootPeerInfo
data RootPeerInfo = RootPeerInfo {

       -- | Should we treat this root peer as persistent or ephemeral in the
       -- known peer set? When the root peer set changes to remove peers,
       -- persistent ones are kept in the known peer set, while ephemeral ones
       -- are removed.
       --
       -- An ephemeral policy is appropriate for peers specified by
       -- configuration, or DNS for singular addresses. A persistent policy
       -- is appropriate for DNS names that correspond to pools of addresses.
       --
       rootPeerEphemeral :: !Bool,

       -- | Should this root peer be advertised to other peers asking for
       -- known peers? For certain peers specified by configuration it would
       -- be an appropriate policy to keep them private.
       --
       rootPeerAdvertise :: !Bool
     }
  deriving (Eq, Show)

data KnownPeerInfo = KnownPeerInfo {
       knownPeerAdvertise :: !Bool,
       knownPeerOther     :: ()
     }
  deriving (Eq, Show)

data TracePeerSelection peeraddr =
       TraceRootPeerSetChanged  (Map peeraddr RootPeerInfo)
     | TraceKnownPeerSetChanged (Map peeraddr KnownPeerInfo)
  deriving (Eq, Show)



-- |
--
peerSelectionGovernor :: (MonadSTM m, MonadFork m, Alternative (STM m),
                          Ord peeraddr)
                      => Tracer m (TracePeerSelection peeraddr)
                      -> PeerSelectionActions peeraddr m
                      -> PeerSelectionPolicy  peeraddr m
                      -> m Void
peerSelectionGovernor tracer
                      actions@PeerSelectionActions {
                        readPeerSelectionTargets
                      }
                      policy =

    withJobPool $ \jobPool ->
      peerSelectionGovernorLoop
        tracer
        actions
        policy
        jobPool
        emptyPeerSelectionState


-- | Our pattern here is a loop with two sets of guarded actions:
--
-- * Actions guarded on predicates on the current immutable state, e.g.
--   * below known peer targets & below in-progress limit
--
-- * Actions guarded by blocking and waiting for state changes, e.g.
--   * root peer set changed
--   * churn timeout
--   * async action completed
--   * established connection failed
--
-- We check the internal actions first, and otherwise the blocking actions.
-- In each case we trace the action, update the state and execute the
-- action asynchronously.
--
peerSelectionGovernorLoop :: forall m peeraddr.
                             (MonadSTM m, MonadFork m, Alternative (STM m),
                              Ord peeraddr)
                          => Tracer m (TracePeerSelection peeraddr)
                          -> PeerSelectionActions peeraddr m
                          -> PeerSelectionPolicy  peeraddr m
                          -> JobPool m (ActionCompletion peeraddr)
                          -> PeerSelectionState peeraddr
                          -> m Void
peerSelectionGovernorLoop tracer
                          PeerSelectionActions{..}
                          PeerSelectionPolicy{..}
                          jobPool =
    loop
  where
    loop :: PeerSelectionState peeraddr -> m Void
    loop st | Just action <- guardedActionsInternal st = do
      st' <- execute action
      -- TODO: assert that we cannot get the same action again in the new state
      -- otherwise that would not guarantee progress and we could loop on the
      -- same thing again.
      loop st'

    loop st = do
      action <- atomically (guardedActionsBlocking st)
      st' <- execute action
      loop st'

    -- All the alternative non-blocking internal actions.
    -- The <|> here is on Maybe.
    guardedActionsInternal :: PeerSelectionState peeraddr
                           -> Maybe (Action m peeraddr)
    guardedActionsInternal st =
          empty
{-
          knownPeersBelowTarget st
      <|> knownPeersAboveTarget st
      <|> establishedPeersBelowTarget st
      <|> establishedPeersAboveTarget st
      <|> activePeersBelowTarget st
      <|> activePeersAboveTarget st
-}
    -- All the alternative potentially-blocking actions.
    -- The <|> here is STM's orElse.
    guardedActionsBlocking :: PeerSelectionState peeraddr
                           -> STM m (Action m peeraddr)
    guardedActionsBlocking st =
          empty
{-
          changedRootPeerSet st
      <|> changedTargets st
      <|> jobCompleted jobPool st
      <|> establishedConnectionFailed st
      <|> timeoutFired st
-}

    execute (Action decision st' job) = do
      traceWith tracer decision
      forkJob jobPool job
      return st'



data Action m peeraddr =
       Action
         -- | A trace event to classify the decision and action
         (TracePeerSelection peeraddr)

         -- | An updated state to use immediately
         (PeerSelectionState peeraddr)

         -- | An 'Job' to execute asynchronously
         (Job m (ActionCompletion peeraddr))

data ActionCompletion peeraddr =
       CompleteGossip -- either failure or result
--   | Complete ...

{-
knownPeersBelowTarget st@PeerSelectionState{
                            knownPeers,
                            inFlightGossipReqs,
                            targets = PeerSelectionTargets {
                                        targetNumberOfKnownPeers
                                      }
                            }

    -- Are we under target for number of known peers?
  | Map.size knownPeers < targetNumberOfKnownPeers

    -- Are we at our limit for number of gossip requests?
  , numInProgressGossipReqs < maxInProgressGossipReqs

    -- Are there any known peers that we can send a gossip request to?
    -- We can only ask ones where we have not asked them within a certain time.
  , let knownPeersAvailableForGossip = ...
  , not (null knownPeersAvailableForGossip)
  = Just (KnownPeersUnderTarget 

        -- Of those, pick a batch of an appropriate size
        -- TODO: capped by local capacity
        let selectedPeers

        -- Fire them all off in one go
        reqs <- mapM_ (async requestPeerGossip) selectedPeers

        -- In the typical case, where most requests return within a short
        -- timeout we want to collect all the responses into a batch and
        -- add them to the known peers set in one go. But if any don't make
        -- the first timeout then they'll be added later when they do reply
        -- or never if we hit the hard timeout.


knownPeersAboveTarget
    -- Are we over target for known peers?
  | Map.size knownPeers > targetNumberOfKnownPeers
  = Just (KnownPeersOverTarget 

  | otherwise
  = Nothing


establishedPeersBelowTarget st

    -- Are we below target for number of established peers?
  | Map.size knownPeers < targetNumberOfKnownPeers


establishedPeersAboveTarget st


activePeersBelowTarget st
    -- Are we below target for number of established peers?
  | 

activePeersAboveTarget st
-}

{-
changedRootPeerSet st 
    go targets@PeerSelectionTargets{..}
       st@PeerSelectionState{rootPeers, knownPeers} =
      assert (invariantPeerSelectionState st) $ do
      -- TODO: cases for doing something immediately

      -- Wait for any state change
      rootPeers' <- atomically $ do
        -- For now, only look for changes in the root peer set
        -- Later we can combine multiple tests with orElse
        rootPeers' <- readRootPeerSet
        check (rootPeers' /= rootPeers)
        return rootPeers'

      let -- Ephemeral peers that we'll be removing from the knownPeers set
          -- and from the established or active sets if necessary.
          vanishingEphemeralPeers =
            Map.filter rootPeerEphemeral $
            Map.difference rootPeers rootPeers'

          knownPeers' =
              -- and add all new root peers
              Map.unionWith mergeRootAndKnownPeer
                            (Map.map rootToKnownPeer rootPeers')
              -- remove old ephemeral root peers
            $ Map.difference knownPeers vanishingEphemeralPeers

      --TODO: when we have established peers and they're ephemeral and
      -- removed then we should presumably disconnect from them.

      traceWith tracer (TraceRootPeerSetChanged rootPeers')
      traceWith tracer (TraceKnownPeerSetChanged knownPeers')

      go targets st {
        rootPeers  = rootPeers',
        knownPeers = knownPeers'
      }

    -- Keep the existing KnownPeer but override things specific to
    -- root peers
    mergeRootAndKnownPeer r k =
      k { knownPeerAdvertise = knownPeerAdvertise r }

    -- Root peer to a KnownPeer with minimal info, as if previously unknown.
    rootToKnownPeer RootPeerInfo{rootPeerAdvertise} =
      KnownPeerInfo {
        knownPeerAdvertise = rootPeerAdvertise,
        knownPeerOther     = ()
      }
-}
{-
changedTargets st
asyncActionCompleted st
establishedConnectionFailed st
timeoutFired st
-}

example1 :: SimM s ()
example1 = do

    rootPeersVar <- newTVarM peers0

    _ <- fork $
      peerSelectionGovernor
        (Tracer (say . show))
        PeerSelectionActions {
          readRootPeerSet = readTVar rootPeersVar,
          readPeerSelectionTargets =
            pure PeerSelectionTargets {
              targetNumberOfKnownPeers       = 10,
              targetNumberOfEstablishedPeers = 4,
              targetNumberOfActivePeers      = 1,
              targetChurnIntervalKnownPeers       = 300,
              targetChurnIntervalEstablishedPeers = 300,
              targetChurnIntervalActivePeers      = 300
            },
          requestPeerGossip = \_ -> return (Time 0, Nothing)
        }
        PeerSelectionPolicy {
          policyPickKnownPeersForGossip = Nothing,

          policyPickColdPeersToPromote  = \_ _ -> return [],
          policyPickWarmPeersToPromote  = \_ _ -> return [],
          policyPickHotPeersToDemote    = \_ _ -> return [],
          policyPickWarmPeersToDemote   = \_ _ -> return [],
          policyPickColdPeersToForget   = \_ _ -> return []
        }
      >> return ()

    threadDelay 1
    atomically $ writeTVar rootPeersVar peers1
    threadDelay 1

  where
    peers0 :: RootPeerSet Int
    peers0 = Map.singleton 1 RootPeerInfo { rootPeerAdvertise = True, rootPeerEphemeral = False }
    peers1 = Map.singleton 2 RootPeerInfo { rootPeerAdvertise = True, rootPeerEphemeral = True }



------------------------
-- Peer churn governor
--

{-
$peer-churn-governor
-}

-- |
--
peerChurnGovernor :: MonadSTM m
                  => PeerSelectionTargets
                  -> m () --Void
peerChurnGovernor _ =
    return ()


-------------------------------
-- Job pool
--

data JobPool m a = JobPool !(TVar m (Set (ThreadId m)))
                           !(TQueue m a)

data Job m a = SingleOutputJob (m a)
             | MultiOutputJob  ((m a -> m ()) -> m a)

withJobPool :: MonadSTM m => (JobPool m a -> m b) -> m b
withJobPool = undefined

forkJob :: (MonadSTM m, MonadFork m) =>JobPool m a -> Job m a -> m ()
forkJob = undefined

jobPoolSize :: MonadSTM m => JobPool m a -> STM m Int
jobPoolSize = undefined

collectJobResult :: MonadSTM m => JobPool m a -> m (Either SomeException a)
collectJobResult = undefined

{-
forkInThreadSet action =
    mask $ \unmask -> do
      tid <- fork $ do
               res <- try action

    mask $ \unmask -> do
      tid <- fork $ unmask $ do
               case action of
                 AsyncActionSingleCompletion a ->
               tid <- myThreadId
               atomically $ writeTQueue complet
      atomically $ modifyTVar' threadsVar (Set.insert tid)


    mask $ \unmask -> do
      tid <- fork $ unmask $ do
               case action of
                 AsyncActionSingleCompletion a ->
               tid <- myThreadId
               atomically $ writeTQueue complet
      atomically $ modifyTVar' threadsVar (Set.insert tid)
    aaction <- case action of
      AsyncActionSingleCompletion a ->
        async a

    -- return updated state
    return st'

forkInThreadSet action =
    mask $ \unmask -> do
      tid <- fork $ do
               res <- try action
-}


-------------------------------
-- DNS root peer set provider
--

data TraceRootPeerSetDns =
       TraceMonitoringDomains [DNS.Domain]
     | TraceWaitingForTTL DNS.Domain DiffTime
     | TraceLookupResult  DNS.Domain [(IPv4, DNS.TTL)]
     | TraceLookupFailure DNS.Domain DNS.DNSError
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
--
-- This action typically runs indefinitely, but can terminate successfully in
-- corner cases where there is nothing to do.
--
rootPeerSetProviderDns :: Tracer IO TraceRootPeerSetDns
                       -> DNS.ResolvConf
                       -> TVar IO (Map DNS.Domain (Map IPv4 RootPeerInfo))
                       -> [DNS.Domain]
                       -> IO ()
rootPeerSetProviderDns tracer resolvConf rootPeersVar domains = do
    traceWith tracer (TraceMonitoringDomains domains)
    unless (null domains) $ do
      rs <- DNS.makeResolvSeed resolvConf
      DNS.withResolver rs $ \resolver ->
        withAsyncAll (map (monitorDomain resolver) domains) $ \asyncs ->
          waitAny asyncs >> return ()
  where
    rootPeerInfo = RootPeerInfo False True --TODO
    monitorDomain resolver domain =
        go 0
      where
        go :: DiffTime -> IO ()
        go !ttl = do
          when (ttl > 0) $ do
            traceWith tracer (TraceWaitingForTTL domain ttl)
            threadDelay ttl
          reply <- lookupAWithTTL resolver domain
          case reply of
            Left  err -> do
              traceWith tracer (TraceLookupFailure domain err)
              go (ttlForDnsError err ttl)

            Right results -> do
              traceWith tracer (TraceLookupResult domain results)
              atomically $ do
                rootPeers <- readTVar rootPeersVar
                let resultsMap :: Map IPv4 RootPeerInfo
                    resultsMap = Map.fromList [ (addr, rootPeerInfo)
                                              | (addr, _ttl) <- results ]
                    rootPeers' :: Map DNS.Domain (Map IPv4 RootPeerInfo)
                    rootPeers' = Map.insert domain resultsMap rootPeers

                -- Only overwrite if it changed:
                when (Map.lookup domain rootPeers /= Just resultsMap) $
                  writeTVar rootPeersVar rootPeers'

              go (ttlForResults results ttl)

    -- Policy for TTL for positive results
    ttlForResults :: [(IPv4, DNS.TTL)] -> DiffTime -> DiffTime

    -- This case says we have a successful reply but there is no answer.
    -- This covers for example non-existent TLDs since there is no authority
    -- to say that they should not exist.
    ttlForResults [] ttl = ttlForDnsError DNS.NameError ttl

    ttlForResults rs _   = clipTTLBelow
                         . clipTTLAbove
                         . (fromIntegral :: Word32 -> DiffTime)
                         $ maximum (map snd rs)

    -- Policy for TTL for negative results
    -- Cache negative response for 3hrs
    -- Otherwise, use exponential backoff, up to a limit
    ttlForDnsError :: DNS.DNSError -> DiffTime -> DiffTime
    ttlForDnsError DNS.NameError _ = 10800
    ttlForDnsError _           ttl = clipTTLAbove (ttl * 2 + 5)

    -- Limit insane TTL choices.
    clipTTLAbove, clipTTLBelow :: DiffTime -> DiffTime
    clipTTLBelow = max 60     -- between 1min
    clipTTLAbove = min 86400  -- and 24hrs


-- | Like 'DNS.lookupA' but also return the TTL for the results.
--
lookupAWithTTL :: DNS.Resolver
               -> DNS.Domain
               -> IO (Either DNS.DNSError [(IPv4, DNS.TTL)])
lookupAWithTTL resolver domain = do
    reply <- DNS.lookupRaw resolver domain DNS.A
    case reply of
      Left  err -> return (Left err)
      Right ans -> return (DNS.fromDNSMessage ans selectA)
      --TODO: we can get the SOA TTL on NXDOMAIN here if we want to
  where
    selectA DNS.DNSMessage { answer } =
      [ (addr, ttl)
      | DNS.ResourceRecord { rdata = DNS.RD_A addr, rrttl = ttl } <- answer ]


withAsyncAll :: [IO a] -> ([Async IO a] -> IO b) -> IO b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action as
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)

example2 :: [DNS.Domain] -> IO ()
example2 domains = do
    rootPeersVar <- newTVarM Map.empty
--    withAsync (observer rootPeersVar Map.empty) $ \_ ->
    (provider rootPeersVar)
  where
    provider rootPeersVar =
      rootPeerSetProviderDns
        (showTracing stdoutTracer)
        DNS.defaultResolvConf
        rootPeersVar
        domains

    observer :: (Eq a, Show a) => TVar IO a -> a -> IO ()
    observer var fingerprint = do
      x <- atomically $ do
        x <- readTVar var
        check (x /= fingerprint)
        return x
      traceWith (showTracing stdoutTracer) x
      observer var x

