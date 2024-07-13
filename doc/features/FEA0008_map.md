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


Examples
--------

Consistency
-----------

Development
-----------

Overland map generation:
- simple case that reads the first terrain data
- subproject with multiple source files
    parameters of the main function to indicate the initial png file
- allow to read terrain data json file (assume correct format for now)
- test the projects (compilation & running internally defined tests)
- type of land
    mapping to a hex grid (correct x & y scale)
- math and random functions
- randomized water flow direction
- amount of water
- detailed height map, amount of sub-scaling of the original hexes
- rounded terrain that breaks if too steep
- lakes, water, sea, swamp, snow, ice
- trees, rock faces, towns, castles, harbour, tower (for now all png images)
- roads (signposts)
- in game editor of original mass & heights?
- fly over & walk around mode
- version for the web that reads the png function with a URL
- a version that lets you upload your own png as a start
- download the result as a 3d world definition

Useful as stand-alone tool/project.

Comments
--------

Changelog
---------