import uniqid from "uniqid";

export function uniqueId(str) {
  return () => uniqid(str);
}
