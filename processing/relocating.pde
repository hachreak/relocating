import com.cage.colorharmony.*;
import processing.net.Client;
import java.util.Map;

// configuration!
String hostname = "127.0.0.1";
int port = 1234;

ColorHarmony colorHarmony = new ColorHarmony(this);
Client stream;
Map<String, Particle> particles = new HashMap<String, Particle>();
PImage x_y, y_z, x_z;

class Particle{
  String name;
  PVector position;
  int[] col;
  PVector best;
  Particle(String newname, PVector pos, String colparticle){
    name = newname;
    position = pos;
    col = colorHarmony.Hex2RGB(colparticle);
  }
}

Client get_stream(){
  return new Client(this, hostname, port);
}

String next(Client stream){
  // read until "\n"
  return stream.readStringUntil(10);
}

Particle[] raw2particle(String raw){
  if(raw == null) return null;
  String[] pieces = split(raw, ' ');
  Particle[] particles = new Particle[2];
  PVector pos = new PVector(int(pieces[2]), int(pieces[3]), int(pieces[4]));
  PVector best = new PVector(int(pieces[5]), int(pieces[6]), int(pieces[7]));
  particles[0] = new Particle(pieces[1], pos, "#FFFF00");
  particles[1] = new Particle("best", best, "#FF0000");
  return particles;
}

void show(PVector pos, int horiz, int vert, int size, int[] col){
  float[] f = pos.array();
  float x = f[horiz];
  float y = f[vert];
  int max = size / 2;
  if(x < max && x > -max && y < max && y > -max){
    pushMatrix();
    stroke(col[0], col[1], col[2]);
    translate(x, y, 0);
    box(1);
    popMatrix();
  }
}

void show_all(Map<String, Particle> particles, int horiz, int vert, int size,
              PImage orientation){
 // show orientation
 int half_size = size / 2;
 image(orientation, -half_size, half_size-60);
 // print particles
 for(Map.Entry<String, Particle> me: particles.entrySet()){
    Particle particle = me.getValue();
    show(particle.position, horiz, vert, size, particle.col);
 }
}

void split(int size){
 line(0, size, 0, 2 * size, size, 0);
 line(size, 0, 0, size, 2 * size, 0);
}

void setup() {
  size(1000, 1000, P3D);
  background(0);
  stroke(255);
  noSmooth();
  frameRate(24);
  stream = get_stream();
  x_y = loadImage("x_y.png");
  x_z = loadImage("x_z.png");
  y_z = loadImage("y_z.png");
}

void draw(){
  while (stream.available() > 0) {
    Particle[] pars = raw2particle(next(stream));
    if (pars != null){
      particles.put(pars[0].name, pars[0]);
      particles.put(pars[1].name, pars[1]);
    }
  }
  background(0);
  scale(1);
  int size = 500;
  split(size);
  int half_size = size/2;
  // show particle and best in [X,Y]
  translate(half_size, half_size, 0);
  show_all(particles, 0, 1, size, x_y);
  // show particle and best in [X,Z]
  translate(size, 0, 0);
  show_all(particles, 0, 2, size, x_z);
  // show particle and best in [Y,Z]
  translate(-size, size, 0);
  show_all(particles, 1, 2, size, y_z);
}
