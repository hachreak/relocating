relocating
==========

Uses [Multilateration](https://en.wikipedia.org/wiki/Multilateration) (MLAT)
and
[particle swarm optimization](https://en.wikipedia.org/wiki/Particle_swarm_optimization)
 (PSO) to resolve object position.

With `relocating` you will be able to know the position of a object in space.
All informations You have come from a set of radio stations (beacons) that can
give a single piece of information about the object:
(because of measurement errors) an approximate distance (radius of the
circumference) of the object from himself.
This is meaning that with a sufficient number of beacons, placed in different
points in space, you'll be able to determine the position with a good
approximation.
The PSO algorithms are used to resolve this equations system.

Try demo
--------

See examples.

Build
-----

    $ rebar3 compile
