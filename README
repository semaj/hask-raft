Raft
====

High Level Approach
-------------------

Files:
RawServer.hs -> The monadic, IO stuff. Start here `serverLoop`
Server.hs -> The actual server code
Message.hs -> The message record & JSON parsing stuff
Utils.hs -> Helper functions, mostly self-explanatory

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

Problems
--------

*******
EDIT: In hour 54 (really, I have a plugin that tracks how much time I spend on projects - I can show you the graphs if youw ant), I discovered the bug. Also, there are 3 hours until the deadline. I was only timing out clients when it *didn't* receive a message. But if it received a message that doesn't update it's time (like a request vote) it wouldn't attempt to time out. This was leading to deadlocks. I'm still fighting with getting the easier tests to accept the message amount while getting the later tests to accept the latency, but I have officially seen every test pass with a relatively high rate of reliability.
*******

There is a non-deterministic bug here somewhere. It is very possible for things to get into a bad state, in which case it will
never elect a new leader and things will fail. Unfortunately I've spent > 50 hours on this project so I'm too burnt out too continue for now...

It cropped up after adding the separate heartbeat functionality - we did this because we found it difficult to coordinate our sending cooldown and our election timeout. If we made the cooldown too high, the simulator complained of too many messages (on the first few tests). But if we ran the later tests (with many drops) it would run fine. If we then raised the cooldown, the first tests were fine but the last ones complained of a high latency. This was by far the biggest issue.

The current implementation is an attempt to meet somewhere in the middle - it's not consistent at all, but I have seen every test pass.

But usually it passes 18/21 of the tests. Usually failing on 2 bring the pains (and the extra credit).

Testing
-------
I created a test file, `Test.hs`, which was mainly for scratchwork and making sure individual functions work. I don't recommend running this, as it's not comprehensive and was used mainly as a sanity check. It was nice to have the ability to run this simulator locally and be able to edit the simulator (and add debugging messages there).

Debug messages helped a bit also.

Overall
-------

A great project - I wish I could find this little bug - I know it's something subtle with leader election (it gets deadlocked in an unelected state).