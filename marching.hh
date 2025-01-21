#pragma once

#include <functional>
#include <geometry.hh>

// Constructs a mesh approximating the zero isosurface of a given scalar function.
// Evaluation area is a cube centered at `center` with edge length `2*size`.

Geometry::TriMesh isosurface(std::function<double(const Geometry::Point3D &)> f,
                             const Geometry::Point3D &center, double size,
                             size_t min_depth, size_t max_depth);

