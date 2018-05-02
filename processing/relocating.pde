import com.cage.colorharmony.*;
import processing.net.Client;
import java.util.Map;

ColorHarmony colorHarmony = new ColorHarmony(this);
Client stream;
Map<String, Particle> particles = new HashMap<String, Particle>();

//Client client; 
String inString;
byte interesting = 10;

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
  return new Client(this, "127.0.0.1", 1234);
}

Particle[] raw2particle(String raw){
  if(raw == null) return null;
  String[] pieces = split(raw, ' ');
  Particle[] particles = new Particle[2];
  particles[0] = new Particle(pieces[1], new PVector(int(pieces[2]), int(pieces[3]), int(pieces[4])), "#FFFF00");
  particles[1] = new Particle("best", new PVector(int(pieces[5]), int(pieces[6]), int(pieces[7])), "#FF0000");
  return particles;
}

String next(Client stream){
  return stream.readStringUntil(interesting);
}

void show(PVector pos, int[] col){
  pushMatrix();
  stroke(col[0], col[1], col[2]);
  translate(pos.x, pos.y, pos.z);
  box(1);
  popMatrix(); 
}

void show_all(Map<String, Particle> particles){
 // print particles 
 for(Map.Entry<String, Particle> me: particles.entrySet()){
    Particle particle = me.getValue();
    show(particle.position, particle.col);
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
  Particle[] pars = raw2particle(next(stream));
  if (pars != null){
    particles.put(pars[0].name, pars[0]);
    particles.put(pars[1].name, pars[1]);
    background(0);
    scale(2);
    translate(200, 200, 0);
    // show particle and best
    show_all(particles);
  }
}
