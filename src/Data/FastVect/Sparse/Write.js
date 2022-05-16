export const emptyImpl = new Set();
export const insertImpl = (i) => (s) => {
  const r = new Set(s);
  r.add(i);
  return r;
}
export const hasImpl = (i) => (s) => s.has(i);