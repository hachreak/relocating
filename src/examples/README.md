How to visualize tracking
=========================

Open Erlang console:

```bash
$> rebar3 shell --apps relocating
1> Beacons1 = [{{0,0,0}, 50}, {{60,60,0}, 35}, {{80,0,0}, 57}]
2> Beacons2 = [{{0,0,0}, 200}, {{60,60,0},155}, {{80,0,0}, 122}].
3> Pid = relocating_engine_example_mlat:run("fuu", , 20, -1, 20).

Open `processing/relocating.pde` file with
[Processing](https://processing.org/) editor.

Now a screen will show particle tracking: red point is the best position found,
yellow points show best position of every particle.

Now, come back to erlang console to simulate a movement by the particle
tracked:

```bash
4> relocating_env:ctx(Pid, [{set, beacons, Beacons2}, reset]).
```

Automatically, the system is reset and start to find new best position.

Or, you can read the best position found:

  5> relocating_env:get_best(Pid).
