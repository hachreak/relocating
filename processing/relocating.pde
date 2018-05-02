import com.cage.colorharmony.*;

ColorHarmony colorHarmony = new ColorHarmony(this);

import java.util.Map;

String[] stream;
int index = 0;

Map<String, Particle> particles = new HashMap<String, Particle>();

class Particle{
  String name;
  PVector position;
  int[] col;
  PVector best;
  Particle(String newname, PVector pos, PVector newbest){
    name = newname;
    position = pos;
    best = newbest;
    col = colorHarmony.Hex2RGB("#FFFF00");
    //colorHarmony.Hex2RGB(colorHarmony.RandomHexColor());
  }
}

String[] get_stream(){
  index = 0;
  String filename = "tracking.log";
  return loadStrings(filename);
}

Particle raw2particle(String raw){
  if(raw == null) return null;
  String[] pieces = split(raw, ' ');
  return new Particle(
    pieces[1],
    new PVector(int(pieces[2]), int(pieces[3]), int(pieces[4])),
    new PVector(int(pieces[5]), int(pieces[6]), int(pieces[7]))
  );
}

String next(String[] stream){
  if (index < stream.length) {
    String raw = stream[index];
    index++;
    return raw;
  }
  return null;
}

void show(PVector pos, int[] col){
  pushMatrix();
  stroke(col[0], col[1], col[2]);
  translate(pos.x, pos.y, pos.z);
  box(1);
  popMatrix();
}

void show_all(Map<String, Particle> particles, PVector best){
 for(Map.Entry<String, Particle> me: particles.entrySet()){
    // print particles
    Particle particle = me.getValue();
    show(particle.position, particle.col);
    // print best
    int[] c = {255, 0, 0};
    show(best, c);
 }
}

void setup() {
  size(1000, 1000, P3D);
  background(0);
  stroke(255);
  noSmooth();
  frameRate(12);
  stream = get_stream();
}

void draw(){
  Particle particle = raw2particle(next(stream));
  if (particle != null){
    particles.put(particle.name, particle);
    background(0);
    scale(2);
    translate(200, 200, 0);
    // show particle and best
    show_all(particles, particle.best);
  }
}
