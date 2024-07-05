Scripting
=========

Summary
-------

State
-----
- **Complete** No
- **Implementation** Partial
- **Tested** Limited
- **Compatible** Not enough
- **Documentation** No
- **Release** Initial project release
- **Stable** No

Use-cases
---------

Design
------
struct Point {
    r: integer limit(0, 255) not null,
    g: integer limit(0, 255) not null,
    b: integer limit(0, 255) not null,
    value: virtual(r * 0x10000 + g * 0x100 + b)
}

struct Image {
    name: text,
    width: integer,
    height: integer,
    data: vector<Point>
}

Examples
--------

Consistency
-----------

Development
-----------
Overland map generation:
- integrate png in language:
    image: name, width, height, data
    language as part of the library
- type of land (rough features from png?)
    mapping to a hex grid without distortion (scale)
- randomized water flow direction
- amount of water
- detailed height map, amount of sub-scaling of the original hexes
- rounded terrain that breaks if too steep
- lakes, water, sea, swamp, snow, ice
- trees, rock faces, towns, castles, harbour, tower (for now all png images)
- roads (signposts)
- in game editor of original mass & heights?
- fly over & walk around mode

Useful as stand-alone tool/project.

Comments
--------

Changelog
---------