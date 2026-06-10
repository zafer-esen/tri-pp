/* The for-loop declaration spans two lines; extracting it in front of
   the loop inserts a newline, so markers must re-sync the lines after
   it. (The decl's own continuation line merges into the for's line;
   lines are the unit of fidelity, so no probe sits there.) */
int main() {
  int probe_n = 5;
  int probe_acc = 0;
  for (int probe_i = 0,
       j = 1;
       probe_i < probe_n; probe_i++) {
    probe_acc += probe_i * j;
  }
  int probe_after = probe_acc;
  return probe_after;
}
