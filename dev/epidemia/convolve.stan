/**
 * Return a [size(x) + size(y) - 1, 5] integer array of indices for convolution.
 * The first column is the index of the output; the remaining are starting and
 * ending indices for x and y at each convolution step.
 *
 * @param x Primary vector to be convolved
 * @param y Secondary vector to be convolved
 *
 * @return A [size(x) + size(y) - 1, 5] integer array with one row for each
 *   convolution output and columns:
 *      [,1]: Output index
 *      [,2]: Starting x index
 *      [,3]: Ending x index
 *      [,4]: Starting y index
 *      [,5]: Ending y index
 */
int[ , ] conv_index(vector x, vector y) {
    int N_x = rows(x);
    int N_y = rows(y);
    int N   = N_x + N_y - 1;

    int index[N, 5];

    for (n in 1:N) {
      index[n, 1] = n;
      index[n, 2] = min(max(1, n), N_x);
      index[n, 3] = min(n, N_x);
      index[n, 4] = min(max(1, n), N_y);
      index[n, 5] = min(n, N_y);
    }

    return index;
  }

vector convolve(vector x, vector y, int[ , ] I) {

  vector[dims(I)[1]] c;

  for (i in I) {
    c[i[1]] = dot_product(
      segment(x, i[2], i[3]),
      reverse(segment(y, i[4], i[5]))
    );
  }

  return c;
}
