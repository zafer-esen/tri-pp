/* Expansion combined with --line-markers: the for-loop extraction below
   changes the line count, the annotations keep theirs; the probes must
   map to their original lines. */
#define LIMIT 240
#define POS(v) ((v) > 0)
int main() {
  int probe_n = 5;
  int probe_acc = 0;
  /*@ loop invariant POS(probe_n) &&
    @   probe_acc <= LIMIT;
    @*/
  for (int probe_i = 0,
       j = 1;
       probe_i < probe_n; probe_i++) {
    probe_acc += probe_i * j;
  }
  //@ assert probe_acc < LIMIT && POS(probe_n);
  int probe_after = probe_acc;
  return probe_after;
}
