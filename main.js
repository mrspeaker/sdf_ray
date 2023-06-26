/*
  Canvas SDF by Mr Speaker
  
  Canvas implementation of Jamie Wong's SDF raymarching tutorial
  https://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/

  Warning: canvas is very slow compared to webgl ;)
*/

const MAX_MARCHING_STEPS = 50;
const MIN_DIST = 1.0;
const MAX_DIST = 30.0;
const EPSILON = 0.1;

function main() {
  const c = document.getElementById("board").getContext("2d");
  const img = c.getImageData(0, 0, c.canvas.width, c.canvas.height);
  setInterval(() => render(c, img), 33);
}

function render(c, img) {
  const w = img.width;
  const h = img.height;

  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const fragCol = frag([x, y], [w, h]);
      set_pixel(img, fragCol, x, y);
    }
  }

  c.putImageData(img, 0, 0);
}

const frag = (pos, res) => {
  const t = Date.now() / 1000;

  const viewDir = rayDir((65 / 180) * Math.PI, res, pos);
  const eye = [
    Math.sin(t * 0.05) * 10 + 20,
    Math.sin(t * 0.05) * 10.4 + 10,
    20 + Math.cos(t * 0.08) * 2,
  ];
  const viewToWorld = viewMatrix(eye, [0, 0, 0], [0, -1, 0]);
  const worldDir = mul_m4v4(viewToWorld, [...viewDir, 0]);

  const d = march(eye, worldDir, MIN_DIST, MAX_DIST);
  if (d > MAX_DIST - EPSILON) {
    return [0, 0, 0];
  }

  // The closest point on the surface to the eyepoint along the view ray
  const p = add(eye, mul_s(worldDir, d));
  const K_a = abs([Math.sin(t * 0.24), Math.cos(t * 0.16), Math.sin(t * 0.2)]);
  const K_d = [0.4, 0.7, 0.2];
  const K_s = [1.0, 1.0, 1.0];
  const shininess = 10.0;

  const color = phong(K_a, K_d, K_s, shininess, p, eye);
  const fg = 1 - d / MAX_DIST;
  return mul_s(
    [
      color[0] * Math.pow(fg, 4),
      color[1] * Math.pow(fg, 4),
      color[2] * Math.pow(fg, 2),
    ],
    255
  );
};

const sphere = (p) => length(p) - 1.0;

//  width = height = length = 2.0
const cube = (p) => {
  const d = sub(abs(p), [1, 1, 1]);
  const inside = Math.min(Math.max(d[0], Math.max(d[1], d[2])), 0);
  const outside = length(max(d, [0, 0, 0]));
  return inside + outside;
};

const intersect = (d1, d2) => Math.max(d1, d2);
const union = (d1, d2) => Math.min(d1, d2);
const diff = (d1, d2) => Math.max(d1, -d2);
const rep = (p, c, sdf_f) => {
  const hc = mul_s(c, 0.5);
  const q = sub(mod(add(p, hc), c), hc);
  return sdf_f(q);
};

const scale = (s, sdf_f) => (p) => sdf_f(mul_s(p, s)) / s;
const disp1 = (p) =>
  Math.sin(0.1 * p[0]) * Math.sin(20 * p[1]) * Math.sin(41 * p[2]);
const displace = (sdf_f, disp_f) => (p) => {
  const d1 = sdf_f(p);
  const d2 = disp_f(p);
  return d1 + d2;
};

const blob = (p) => {
  const t = Math.abs(Math.sin(Date.now() / 10000) * 1.2) + 0.5;
  const s1 = sphere(div_s(p, t)) * t;
  const c1 = cube(add(p, vec3(0, Math.sin(Date.now() / 1000), 0)));
  return intersect(c1, s1);
};

const scene = (p) => {
  return rep(p, [1, 1, 1], displace(scale(4, blob), disp1));
};

const march = (eye, dir, min, max) => {
  let depth = min;
  for (let i = 0; i < MAX_MARCHING_STEPS; i++) {
    const d = scene([
      eye[0] + depth * dir[0],
      eye[1] + depth * dir[1],
      eye[2] + depth * dir[2],
    ]);
    if (d < EPSILON) return depth;
    depth += d;
    if (depth >= max) return max;
  }
  return max;
};

const rayDir = (fov, size, pos) => {
  const x = pos[0] - size[0] / 2.0;
  const y = pos[1] - size[1] / 2.0;
  const z = size[1] / Math.tan(fov / 2.0);
  return normalize([x, y, -z]);
};

const viewMatrix = (eye, target, up) => {
  const fwd = normalize(sub(target, eye));
  const side = normalize(cross(fwd, up));
  const up2 = cross(side, fwd);
  return [
    [side[0], side[1], side[2], 0],
    [up2[0], up2[1], up2[2], 0],
    [-fwd[0], -fwd[1], -fwd[2], 0],
    [0, 0, 0, 1],
  ];
};

const estimateNormal = (p) =>
  normalize([
    scene([p[0] + EPSILON, p[1], p[2]]) - scene([p[0] - EPSILON, p[1], p[2]]),
    scene([p[0], p[1] + EPSILON, p[2]]) - scene([p[0], p[1] - EPSILON, p[2]]),
    scene([p[0], p[1], p[2] + EPSILON]) - scene([p[0], p[1], p[2] - EPSILON]),
  ]);

const phongLight = (k_d, k_s, alpha, p, eye, lightPos, lightIntensity) => {
  const N = estimateNormal(p);
  const L = normalize(sub(lightPos, p));
  const V = normalize(sub(eye, p));
  const R = normalize(reflect(invert(L), N));

  const dotLN = dot(L, N);
  const dotRV = dot(R, V);

  if (dotLN < 0) return [0, 0, 0];
  if (dotRV < 0) {
    // Light reflection in opposite direction as viewer,
    // apply only diffuse component
    return mul(lightIntensity, mul_s(k_d, dotLN));
  }
  const spec = mul_s(k_s, Math.pow(dotRV, alpha));
  const diff = mul_s(k_d, dotLN);
  const m2 = mul(lightIntensity, add(diff, spec));

  return m2;
};

const phong = (k_a, k_d, k_s, alpha, p, eye) => {
  const t = Date.now() / 1000;

  const ambientLight = mul_s([1, 1, 1], 0.5);
  const color = mul(ambientLight, k_a);
  const light1Pos = add(eye, [
    3 * Math.sin(t * 0.9),
    3 * Math.sin(t * 0.21),
    3 * Math.cos(t * 0.9),
  ]);
  const light1Intensity = [0.9, 0.4, 0.4];
  const ph1 = phongLight(k_d, k_s, alpha, p, eye, light1Pos, light1Intensity);

  return add(ph1, color);
};

// vec3 helpers
const vec3 = (a, b, c) => [a, b, c];
const length = (v) => Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
const add = (a, b) => [a[0] + b[0], a[1] + b[1], a[2] + b[2]];
const sub = (a, b) => [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
const mul = (a, b) => [a[0] * b[0], a[1] * b[1], a[2] * b[2]];
const add_s = (v, s) => [v[0] + s, v[1] + s, v[2] + s];
const mul_s = (v, s) => [v[0] * s, v[1] * s, v[2] * s];
const div_s = (v, s) => [v[0] / s, v[1] / s, v[2] / s];
const mod = (a, b) => [a[0] % b[0], a[1] % b[1], a[2] % b[2]];
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
const cross = (a, b) => [
  a[1] * b[2] - a[2] * b[1],
  a[2] * b[0] - a[0] * b[2],
  a[0] * b[1] - a[1] * b[0],
];
// I - 2.0 * dot(N, I) * N, where I is the incident vector and n is normal
const reflect = (v, n) => sub(v, mul_s(n, 2 * dot(n, v)));
const invert = (v) => [-v[0], -v[1], -v[2]];
const normalize = (v) => {
  const mag = length(v);
  return [v[0] / mag, v[1] / mag, v[2] / mag];
};
const abs = (v) => [Math.abs(v[0]), Math.abs(v[1]), Math.abs(v[2])];
const max = (a, b) => [
  Math.max(a[0], b[0]),
  Math.max(a[1], b[1]),
  Math.max(a[2], b[2]),
];
const mul_m4v4 = (m, v) => [
  m[0][0] * v[0] + m[1][0] * v[1] + m[2][0] * v[2] + m[3][0] * v[3],
  m[0][1] * v[0] + m[1][1] * v[1] + m[2][1] * v[2] + m[3][1] * v[3],
  m[0][2] * v[0] + m[1][2] * v[1] + m[2][2] * v[2] + m[3][2] * v[3],
  m[0][3] * v[0] + m[1][3] * v[1] + m[2][3] * v[2] + m[3][3] * v[3],
];

function set_pixel(img, c, x, y) {
  const pix = img.data;
  const off = (y * img.width + x) * 4;
  pix[off + 0] = c[0];
  pix[off + 1] = c[1];
  pix[off + 2] = c[2];
  pix[off + 3] = 255;
}

main();
