# voxelmap

A program that takes in mathematical descriptions and makes layer-by-layer guides for minecraft and other voxel-based games.

It also generates a `.litematic` file for the [Litematica minecraft mod](https://github.com/maruohon/litematica).

## Build

> `cargo build --release`

## Options

```
voxelmap 0.1.0
raffitz <raf.a.m.c.gon@gmail.com>
Converts mathematical descriptions of objects to voxel maps

USAGE:
    voxelmap [FLAGS] [OPTIONS] <FILE> <OUTPUT_DIR>

FLAGS:
    -d, --debug      Show parsing steps
    -h, --help       Prints help information
    -o, --offset     Offset the computation by half a block
    -t, --test       Parses the input file, does not output
    -V, --version    Prints version information

OPTIONS:
    -b, --block <block>    The minecraft block to be used in the Litematica output, defaults to minecraft:stone
    -s, --scale <N>        The scale parameter for the object

ARGS:
    <FILE>          The file describing the shape to map
    <OUTPUT_DIR>    The folder where the output images will be stored
```

## Output

The output consists of a series of images, each depicting a layer of the solid, as well as a `.litematic` file.

## Current Solid Examples

| File | Name | Description |
| --- | --- | --- |
| `cube.solid` | Cube | A cube stood on its vertex |
| `hcube.solid` | Hollow Cube | A hollow cube stood on its vertex |
| `tetrahedron1.solid` | Tetrahedron | A tetrahedron stood on its edge |
| `htetra1.solid` | Hollow Tetrahedron | A hollow tetrahedron stood on its edge |
| `tetrahedron2.solid` | Tetrahedron | A tetrahedron stood on its base/vertex |
| `htetra2.solid` | Hollow Tetrahedron | A hollow tetrahedron stood on its base/vertex |
| `tetrahedron3.solid` | Tetrahedron | A tetrahedron stood on its base/vertex |
| `htetra3.solid` | Hollow Tetrahedron | A hollow tetrahedron stood on its base/vertex |
| `octahedron1.solid` | Octahedron | An octaheron stood on its vertex |
| `hoctahedron1.solid` | Hollow Octahedron | A hollow octahedron stood on its vertex |
| `sphere.solid` | Sphere | A sphere of radius s |
| `torus1.solid` | Torus | A torus with a tube radius of s, and a hole radius of 2 * s |
| `ramp.solid` | Ramp | A spiraling ramp |
| `helix1.solid` | Helix | A helix |
| `hhelix1.solid` | Hollow Helix | A hollow helix |
| `rotating_triangle_torus.solid` | RTT | A triangle rotating 2π around the z axis |
| `hollow_rtt.solid` | RTT | A hollow triangle rotating 2π around the z axis |
| `rotating_triangle_torus2.solid` | RTT | A triangle rotating 4π around the z axis |
| `hollow_rtt_t6.solid` | RTT | A hollow triangle rotating 4π around the z axis |
| `rotating_triangle_torus3.solid` | RTT | A triangle rotating 2π/3 around the z axis |
| `hollow_rtt_t1.solid` | RTT | A hollow triangle rotating 2π/3 around the z axis |
| `rotating_triangle_torus4.solid` | RTT | A triangle rotating 4π/3 around the z axis |
| `hollow_rtt_t2.solid` | RTT | A hollow triangle rotating 4π/3 around the z axis |

## Example execution

```sh

cargo run --release -- -s 10 --block 'minecraft:red_nether_brick' hollow_rtt.solid hrtt3

```
