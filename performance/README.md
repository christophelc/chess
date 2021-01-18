Description
===========

The aim of this documument is to keep track of the different solutions
we tried and compare their performance impact.

Estimation
==========

The measures done on the JVM during a short period of time (a few
minutes) can be inaccurate. When they look unrealistic, we just stop and
restart the program.

Storage
=======

It concerns:

-   Pieces inside Chessboard

-   Moves generated

Until now, we will only use immutable data. The priority is not
optimization at any price. We will first improve the code quality and
make it flexible. We want to try different ideas. We just want to
improve as much as possible the speed and improve the design so that we
can replace easily any part of the program by another one.

Pieces storage
--------------

We tried:

1- Seq\[Piece\] 2- 6 x Seq\[Piece\], one per type of piece 3-
StorageImpl\[PieceId, Piece\] which is like a Map\[PieceId, Piece\]

The 2 is 2 times slower than the first. The t The one is 2 times slower
than the first. The last one is very slow. In fact, the Map used is
immutable and since we are modifying a lot the pieces stored, there is a
lot of times passed to duplicate data.

Moves storage
-------------

We are not only storing moves but control over the squares too. Keeping
track of the squares control induce of course a storage overhead.

We tried:

1- a Seq\[GenericMove\] for control square. We do not store the another
Seq\[GenericMove\] use for real moves since they are generated on the
fly and only stored in the Trees of the Engines. 2- Seq\[GenericMove\]
where here GenericMove contains Tags. A tag defines if a GenericMove
means a real move or a control square

The both the solution give almost the same results. The second solution
is far more powerful since it will give us the possibility to genrate
move by difference, one move after the over. We are expecting a average
gain between 6x and 10x per move. We will see.
