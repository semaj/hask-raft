Raft
====

A rudimentary implementation of the Raft distributed consensus protocol.

https://raft.github.io/

High Level Approach
-------------------

Files:
- RawServer.hs -> The monadic, IO stuff. Start here `serverLoop`
- Server.hs -> The actual server code
- Message.hs -> The message record & JSON parsing stuff
- Utils.hs -> Helper functions, mostly self-explanatory

The server keeps track of the following important things:
- Who is the leader
- What messages it needs to send and to whom
- When the last time we sent the above messages was
- Our log, and what we've applied so far

Election occurs as specified in Raft - candidate times out, receives votes, then transitions.

The important implementation details are in how we keep track of messages. We have a map of Server -> Message,
in which we keep the last message we sent / will send to each server. We also keep a map of Server -> Time, which is the
last time we sent the message in the messQ. If the time in timeQ goes over a threshold, we resend the message
in messQ.

Note that these messages are not heartbeats. Hearbeats are sent periodically and are independent of actual updates.
They are sent to everyone and servers do not respond to them.

Also - if a message like that has expired we won't necessarily wait for it before updating that message's commands. We will just add them to be sent on the next round, which is a rather clever form of batching.

Testing
-------
I created a test file, `Test.hs`, which was mainly for scratchwork and making sure individual functions work. I don't recommend running this, as it's not comprehensive and was used mainly as a sanity check. It was nice to have the ability to run this simulator locally and be able to edit the simulator (and add debugging messages there).

What it does & doesn't do
-------------------------

Does:
- Leader election
- Handle up to ~40% message drops
- Log replication
- Handle follower, candidate, and leader failures
- Is very available and relatively consistent key-value store

Does not
- Failures in which crashed servers come back online (theoretically works, but untested)
- Snapshotting when recovery
- Any persistent storage



