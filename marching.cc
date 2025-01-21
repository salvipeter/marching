#include <map>

#include "marching.hh"

using namespace Geometry;

//     3---11----7
//    /|        /|
//   1 |       3 |
//  /  5      /  7
// 2---10----6   |
// |   |     |   |
// |   1----9|---5
// 4  /      6  /
// | 0       | 2
// |/        |/
// 0----8----4

static std::pair<size_t, size_t> edges[12] = {
  {0,1}, {2,3}, {4,5}, {6,7},
  {0,2}, {1,3}, {4,6}, {5,7},
  {0,4}, {1,5}, {2,6}, {3,7}
};

static std::vector<std::array<size_t, 3>> configurations[256] = {
#include "table.inc"
};

TriMesh isosurface(std::function<double(const Point3D &)> f,
                   const Point3D &center, double size,
                   size_t min_depth, size_t max_depth) {
  TriMesh mesh;
  PointVector vertices;

  std::map<size_t, double> value_map;
  std::map<std::pair<size_t, size_t>, size_t> vertex_map;

  size_t res = 1 << (max_depth + 1);
  Vector3D dirs[8] = { {-1,-1,-1}, {-1,-1, 1}, {-1, 1,-1}, {-1, 1, 1},
                       { 1,-1,-1}, { 1,-1, 1}, { 1, 1,-1}, { 1, 1, 1} };
  auto diff = [&](size_t i, size_t delta) {
    const auto &d = dirs[i];
    return
      (d[0] + 1) / 2 * delta * res * res +
      (d[1] + 1) / 2 * delta * res +
      (d[2] + 1) / 2 * delta;
  };

  std::function<void(Point3D, double, size_t, size_t)> march =
    [&](Point3D center, double size, size_t index, size_t depth) {
      size_t delta = res >> depth;
      size_t id[8], config = 0;
      for (size_t i = 0; i < 8; ++i) {
        double v;
        id[i] = index + diff(i, delta);
        if (value_map.contains(id[i]))
          v = value_map[id[i]];
        else {
          auto p = center + dirs[i] * size;
          v = value_map[id[i]] = f(p);
        }
        if (v >= 0)
          config = config * 2 + 1;
        else
          config *= 2;
      }
      if ((config == 0 || config == 255) && min_depth <= depth)
        return;
      if (max_depth > depth) {
        double half = size / 2;
        delta >>= 1;
        for (size_t i = 0; i < 8; ++i) {
          size_t next = index + diff(i, delta);
          march(center + dirs[i] * half, half, next, depth + 1);
        }
      } else {
        for (const auto &tri : configurations[config]) {
          size_t v[3];
          for (size_t i = 0; i < 3; ++i) {
            const auto &edge = edges[tri[i]];
            std::pair<size_t, size_t> key = { id[edge.first], id[edge.second] };
            if (key.first > key.second)
              std::swap(key.first, key.second);
            if (vertex_map.contains(key))
              v[i] = vertex_map[key];
            else {
              v[i] = vertices.size();
              vertex_map[key] = v[i];
              Point3D pa = center + dirs[edge.first] * size;
              Point3D pb = center + dirs[edge.second] * size;
              double a = value_map[id[edge.first]];
              double b = value_map[id[edge.second]];
              double alpha = a / (a - b);
              vertices.push_back(pa * (1 - alpha) + pb * alpha);
            }
          }
          mesh.addTriangle(v[0], v[1], v[2]);
        }
      }
    };

  march(center, size, 0, 0);

  mesh.setPoints(vertices);
  return mesh;
}
