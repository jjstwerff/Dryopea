# Dryopea
Game development on Dryopea rogue AI

This is an example open-source game for the development of the underlying game engine.
The goal is a 3d game that fully runs inside a browser or as a standalone Vulcan executable.

It will feature an overarching tactical campaign with relatively short missions to the planet's surface.
Each mission will have a randomly chosen set of, potentially secret, objectives and a time limit due to increasingly
hard encounters. At any moment the player can decide to break off the mission and try to salvage as much as possible
before leaving.

When the final encounter is cleared, the mission is also over and the player can choose to keep their base occupied
but relatively dormant on the planet. It can then still produce and hinder opposing factions.
It will be possible to defend this base in a future mission against renewed attacks.

It should be greatly extensible with a quality editor for rapid prototyping.
This editor can edit full maps or individual assets used on these maps.
It will be able to load glb files for more in depth assets and animations but should be fully functional without it.

The game will eventually provide multiplayer support for both collaborative or competitive missions.

# Releases
There are feature documents under doc/features/FEA####_name.md. These describe a specific feature or enhancement on
a feature. After the first pre-release of the project all development should be guided via feature documents.

Each feature should be developed in a branch with the same name.
It is possible that a feature gets postponed or even dropped when deemed problematic.

When deemed necessary, parts of features will be split to new documents. So it is not a problem to have a more full
design in a feature.

# Initial command-line prompt
export PS1="\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$"

# Git branch on command line
git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

export PS1="\w\a\[\033[00;32m\]\$(git_branch)\[\033[00m\]\$ "

/usr/lib/git-core/git-sh-prompt

# Roadmap
At the start of each month, there will be a release of the language.
Eventually, when more stable, this will be lowered to a release at the start of each quarter.

The aim is to get a full-featured language:
- Fully implemented iterators over currently defined structures.
- Create integration tests that also function as technical language documentation.
- Polymorphism in data structures.
- Safe threading support
- Fix the more glaring performance issues.
- Library support.
- Data structure migrations
