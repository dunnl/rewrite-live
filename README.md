# Rewrite Live!

## Building

I am building on NixOS. If you not using Nix, basically you will need
install the following three utilities:

- NPM, the Javascript package manager.
- Purescript (`purs`), the programming language
- Spago, the build tool to Purescript

Eventually it would be nice to implement a solution that works as-is
on any system, with the option to use Nix for Nix users.

### Building on NixOS
Ideally, all you need to start building this project is `npm`, which
would take care of install Purescript and Spago. On NixOS, the
Purescript and Spago binaries installed by npm cannot be executed
because the Linux dynamic linker `ld-linux.so` is not in its usual
location (and probably other dependencies too). Therefore a
`shell.nix` is provided for Nix users which installs all three of the
previous systems. If you aren't using Nix, you will need to install
Purescript and Spago yourself using either npm or your system's
package manager.

Regardless, npm is the main tool for handling other things (running
the build script, installing JS packages, etc). You shouldn't need to
run `spago` much except if you are, say, adding new Purescript
dependencies.
