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

See the example `relocating_engine` and run it with:

```bash
rebar3 shell --apps relocating
```

Try the engine with 3 beacons, 6 particle and 90 rounds:


```erlang
1> Pid = relocating_engine:run("hello", [{{0,0,0}, 50}, {{60,60,0}, 35}, {{80,0,0}, 57}], 6, 90).
2> relocating_env:get_best(Pid).
{35.694006126879145,33.75898818844854,9.738399962963765}
```

The result is the best position found.


Build
-----

    $ rebar3 compile
