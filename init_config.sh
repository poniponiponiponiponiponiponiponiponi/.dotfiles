#!/bin/bash

DOTFILES_PATH="$HOME/.dotfiles"

if ! uname -a | grep -q arch; then
    echo "This is an Arch Linux only household"
    exit 1
fi

fonts=(
    ttf-dejavu
    ttf-font-awesome
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    nerd-fonts
    ttf-jetbrains-mono-nerd
)
dev=(
    emacs
    git
    gcc
    gdb
    clang
    musl
    make
    cmake
    bear
    python
    python-pip
    python-virtualenv
    python-uv
    python-poetry
    ipython
    jedi-language-server
    ruby
    rubygems
    nasm
    dotnet-sdk
    dotnet-runtime
    dotnet-host
    jdk-openjdk
    openjdk-doc
    openjdk-src
    tk
    rustup
    bash-completion
    bash-language-server

    man
    man-pages

    qemu-user-static-binfmt
    qemu-user-static
    qemu-full

    riscv32-elf-binutils
    riscv32-elf-gdb
    riscv64-elf-binutils
    riscv64-elf-gcc
    riscv64-elf-gdb
    riscv64-elf-newlib
    riscv64-linux-gnu-binutils
    riscv64-linux-gnu-gcc
    riscv64-linux-gnu-gdb
    riscv64-linux-gnu-glibc
    riscv64-linux-gnu-linux-api-headers
    aarch64-linux-gnu-binutils
    aarch64-linux-gnu-gcc
    aarch64-linux-gnu-gdb
    aarch64-linux-gnu-glibc
    arm-none-eabi-binutils
    arm-none-eabi-gcc
    arm-none-eabi-gdb
)
utility=(
    openssh
    ripgrep
    fd
    dust
    fastfetch
    stow
    screen
    unzip
    unrar
    curl
    htop
    xclip
    zip
    p7zip
    wget
    bc
    tldr
    parallel
    perf
    minicom
    arandr
    keychain
    feh
    dunst
    acpi
    imagemagick
    openbsd-netcat
    plocate
    flatpak
    udiskie
    scrot
)
ctf=(
    one_gadget
    patchelf
    pwninit
    binwalk
    ropper
    python-pwntools
    ltrace
    strace
)
gui_apps=(
    firefox
    gimp
    alacritty
    dmenu
    mpv
    obs-studio
    lxappearance
    corectrl
)
wine=(
    wine-staging
)
random=(
    aspell
    aspell-pl
    aspell-uk
    aspell-en
    xf86-input-evdev
    xorg-xinput
    i3blocks
    i3status
    i3-wm
)

sudo pacman -Syu --noconfirm \
    "${fonts[@]}" \
    "${dev[@]}" \
    "${utility[@]}" \
    "${ctf[@]}" \
    "${gui_apps[@]}" \
    "${wine[@]}" \
    "${random[@]}"

mkdir -p ~/Projects
mkdir -p ~/FOSS
mkdir -p ~/org
mkdir -p ~/Notes

pushd ~/FOSS
[ -d pwndbg ] || git clone https://github.com/pwndbg/pwndbg
[ -d sig-database ] || git clone https://github.com/push0ebp/sig-database
popd

rustup default stable
rustup component add rustfmt
rustup component add clippy
rustup component add rust-analyzer

gem install seccomp-tools

git config --global user.name "poniponiponiponiponiponiponiponiponi"
git config --global user.email "poniponiponiponiponiponiponiponiponiponi@protonmail.com"
git config --global credential.helper store
git config --global core.editor "emacsclient"
git config --global init.defaultBranch main
git config --global log.decorate full
git config --global core.pager cat
git config --global remote.origin.followRemoteHead warn
git config --global pull.ff only
git config --global status.submodulesummary 1
git config --global submodule.recurse true

sudo localectl set-x11-keymap pl
sudo localectl set-x11-keymap "" "" "" ctrl:nocaps

echo "blacklist pcspkr" | sudo tee /etc/modprobe.d/nobeep.conf
echo "blacklist snd_pcsp" | sudo tee -a /etc/modprobe.d/nobeep.conf