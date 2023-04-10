# Setup LSP

using Pkg
pkgs = ["LanguageServer"]
for pkg in pkgs
    if Base.find_package(pkg) === nothing
        Pkg.add(pkg)
    end
end
