export const decimalToFraction = decimal => {
  if (decimal % 1 === 0) {
    return {
      numerator: decimal,
      denominator: 1
    };
  }

  let gcd = (a, b) => (b ? gcd(b, a % b) : a);

  let len = decimal.toString().length - 2;
  let denominator = Math.pow(10, len);
  let numerator = decimal * denominator;

  let divisor = gcd(numerator, denominator);

  return {
    numerator: numerator / divisor,
    denominator: denominator / divisor
  };
};
