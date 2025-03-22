#include <cmath>
#include <limits>
#include <random>

#include "marching.hh"

using namespace Geometry;
using MarchingCubes::isosurface;

double chebyshev(size_t n, double x) {
  if (n == 0)
    return 1;
  if (n == 1)
    return x;
  return 2 * x * chebyshev(n - 1, x) - chebyshev(n - 2, x);
}

enum Platonic { OCTAHEDRON = 0, DODECAHEDRON = 1, ICOSAHEDRON = 2 };

auto platonic(Platonic type) {
  double f = (1 + std::sqrt(5)) / 2, m = -f, f1 = 1 / f, m1 = -f1;
  static const VectorVector normals[] = {
    { {1,1,1}, {1,1,-1}, {1,-1,1}, {1,-1,-1},
      {-1,1,1}, {-1,1,-1}, {-1,-1,1}, {-1,-1,-1} },
    { {0,1,f}, {0,1,m}, {0,-1,f}, {0,-1,m},
      {1,f,0}, {1,m,0}, {-1,f,0}, {-1,m,0},
      {f,0,1}, {f,0,-1}, {m,0,1}, {m,0,-1} },
    { {1,1,1}, {1,1,-1}, {1,-1,1}, {1,-1,-1},
      {-1,1,1}, {-1,1,-1}, {-1,-1,1}, {-1,-1,-1},
      {0,f,f1}, {0,f,m1}, {0,m,f1}, {0,m,m1},
      {f,f1,0}, {f,m1,0}, {m,f1,0}, {m,m1,0},
      {f1,0,f}, {f1,0,m}, {m1,0,f}, {m1,0,m} }
  };
  double radii[] = {
    1 / std::sqrt(3),
    f / std::sqrt(9 - 3 * f),
    (f + 1) / std::sqrt(3 * (f + 2))
  };
  auto ns = normals[type];
  for (auto &n : ns)
    n.normalize();
  auto r = radii[type];
  return [=](const Point3D &p) {
    auto x = -std::numeric_limits<double>::max();
    for (const auto &n : ns)
      x = std::max(x, (p - (n * r)) * n);
    return x;
  };
}

int main() {
  // See also: https://mathworld.wolfram.com/AlgebraicSurface.html

  // Cube
  auto f0 = [](const Point3D &p) {
    return std::max(std::max(std::abs(p[0]), std::abs(p[1])), std::abs(p[2])) - 1;
  };

  // Entzensberger star
  auto f1 = [](const Point3D &p) {
    auto x = p[0] * p[0], y = p[1] * p[1], z = p[2] * p[2];
    return 400 * (x * y + y * z + x * z) - std::pow(1 - x - y - z, 3);
  };

  // Banchoff-Chmutov surfaces (n = 4, 6, etc.)
  auto f2 = [](size_t n) {
    return [=](const Point3D &p) {
      return chebyshev(n, p[0]) + chebyshev(n, p[1]) + chebyshev(n, p[2]);
    };
  };

  // Klein bottle
  auto f3 = [](const Point3D &p) {
    auto x = p[0] * p[0], y = p[1] * p[1], z = p[2] * p[2];
    auto tmp = x + y + z - 2 * p[1] - 1;
    return (x + y + z + 2 * p[1] - 1) * (tmp * tmp - 8 * z) + 16 * p[0] * p[2] * tmp;
  };

  // Hunt's surface
  auto f4 = [](const Point3D &p) {
    auto x = p[0] * p[0], y = p[1] * p[1], z = p[2] * p[2];
    return 4 * std::pow(x + y + z - 13, 3) + 27 * std::pow(3 * x + y - 4 * z - 12, 2);
  };

  // Voronoi structure in a sphere (e.g. n = 20, thickness = 0.05)
  auto f5 = [](size_t n, double thickness) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(-1, 1);
    std::vector<Point3D> points;
    for (size_t i = 0; i < n; ++i)
      points.emplace_back(dis(gen), dis(gen), dis(gen));
    return [&](const Point3D &p) {
      auto min1 = std::numeric_limits<double>::max(), min2 = min1;
      size_t mini1, mini2;
      for (size_t i = 0; i < n; ++i) {
        const auto &q = points[i];
        auto d = (p - q).normSqr();
        if (d < min1) {
          min2 = min1; mini2 = mini1;
          min1 = d; mini1 = i;
        } else if (d < min2) {
          min2 = d; mini2 = i;
        }
      }
      auto v = (points[mini2] - points[mini1]).normalize();
      min1 = std::abs((p - points[mini1]) * v);
      min2 = std::abs((p - points[mini2]) * v);
      auto voronoi = std::abs(min1 - min2) - thickness;
      auto sphere = p.norm() - 1;
      auto shell = std::max(sphere, -2 * thickness - sphere);
      return std::min(std::max(sphere, voronoi), shell);
    };
  };

  // Hyperbolized Platonic solids
  auto f6 = [](Platonic type, double hyperbolicness) {
    auto f = platonic(type);
    return [=](const Point3D &p) {
      return f(p * std::pow(p.norm(), -hyperbolicness));
    };
  };

  // Chair surface (`a` controls how the center sinks in and form a hole; good values: 0.7-1.0
  //                `b` controls the size of the hole; good values: 0.5-1.0)
  auto f7 = [](double a, double b) {
    return [=](const Point3D &p) {
      auto x = p[0], y = p[1], z = p[2];
      return std::pow(p.normSqr() - a, 2) -
        b * (std::pow(z - 1, 2) - 2 * x * x) * (std::pow(z + 1, 2) - 2 * y * y);
    };
  };

  Point3D center(0, 0, 0);
  // auto mesh = isosurface(f0, center, 1.1, 4, 8);
  // auto mesh = isosurface(f1, center, 1, 4, 8);
  // auto mesh = isosurface(f2(6), center, 3, 4, 8);
  // auto mesh = isosurface(f3, center, 4.1, 4, 8);
  // auto mesh = isosurface(f4, center, 4, 7, 8);
  // auto mesh = isosurface(f5(40, 0.05), center, 1.1, 7, 7);
  // auto mesh = isosurface(f6(Platonic::ICOSAHEDRON, 0.6), center, 1.1, 4, 8);
  auto mesh = isosurface(f7(0.8, 0.6), center, 1.1, 4, 7);
  mesh.writeOBJ("/tmp/mc.obj");
}
