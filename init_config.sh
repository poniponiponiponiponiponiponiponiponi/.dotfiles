#!/bin/bash

DOTFILES_PATH="$HOME/.dotfiles"


if [[ "`uname -a`" == *"arch"* ]]; then
    sudo pacman -Syu emacs firefox ttf-dejavu unzip mdbook musl screen \
         noto-fonts noto-fonts-cjk noto-fonts-emoji nerd-fonts gimp tldr \
         gcc python python-pip zip p7zip wget git curl radare2 r2ghidra bc \
         openbsd-netcat ipython ruby rubygems mpv tmux fish ropper keychain \
         python-pwntools clang htop bat patchelf perf nasm binwalk minicom \
         ltrace strace unrar rustup dunst feh acpi python-r2pipe parallel \
         ttf-font-awesome lxappearance wine-staging xclip eza ripgrep dust \
         zsh alacritty gdb make cmake bear bash-completion man man-pages \
         qemu-full jdk-openjdk openjdk-doc openjdk-src tk fastfetch flatpak \
         riscv32-elf-binutils riscv32-elf-gdb riscv32-elf-newlib obs-studio \
         riscv64-elf-binutils riscv64-elf-gcc riscv64-elf-gdb openssh stow \
         riscv64-elf-newlib riscv64-linux-gnu-binutils bash-language-server \
         riscv64-linux-gnu-gcc riscv64-linux-gnu-gdb python-poetry corectrl \
         riscv64-linux-gnu-glibc riscv64-linux-gnu-linux-api-headers \
         aarch64-linux-gnu-binutils aarch64-linux-gnu-gcc libreoffice-still \
         aarch64-linux-gnu-gdb aarch64-linux-gnu-glibc jedi-language-server \
         arm-none-eabi-binutils arm-none-eabi-gcc arm-none-eabi-gdb \
         python-virtualenv scrot fd one_gadget pwninit plocate aspell \
         aspell-pl aspell-uk aspell-en rizin rz-ghidra python-rzpipe \
         qemu-user-static-binfmt qemu-user-static rofi dmenu arandr \
         ttf-jetbrains-mono-nerd dotnet-sdk dotnet-runtime dotnet-host \
         imagemagick
fi


declare -a commands=("git" "gem" "pip" "curl")
for command in "${commands[@]}"
do
    if ! command -v "$command" &> /dev/null; then
        echo "NO ${command} DETECTED. Please make sure you have everything" \
            "installed before proceeding."
        exit 1
    fi
done

mkdir -p ~/.config
mkdir -p ~/.config/mpv
mkdir -p ~/.config/tmux
mkdir -p ~/.config/i3
mkdir -p ~/.config/dunst
mkdir -p ~/Projects
mkdir -p ~/FOSS
mkdir -p ~/.scripts
mkdir -p ~/org
mkdir -p ~/.emacs.d

ln -sf "$DOTFILES_PATH/mpv.conf" ~/.config/mpv/mpv.conf
ln -sf "$DOTFILES_PATH/pwninit_template.py" ~/.config/pwninit_template.py
ln -sf "$DOTFILES_PATH/tmux.conf" ~/.config/tmux/tmux.conf
ln -sf "$DOTFILES_PATH/alacritty.toml" ~/.config/alacritty.toml
ln -sf "$DOTFILES_PATH/init.el" ~/.emacs.d/init.el
ln -sf "$DOTFILES_PATH/.bashrc" ~/.bashrc
ln -sf "$DOTFILES_PATH/i3_config" ~/.config/i3/config
ln -sf "$DOTFILES_PATH/picom.conf" ~/.config/picom.conf
ln -sf "$DOTFILES_PATH/dunstrc" ~/.config/dunst/dunstrc

git clone https://github.com/pwndbg/pwndbg ~/FOSS/pwndbg
git clone https://github.com/push0ebp/sig-database ~/FOSS/sig-database
git clone https://github.com/rexim/simpc-mode ~/FOSS/simpc-mode
git clone https://aur.archlinux.org/paru.git ~/FOSS/paru
git clone https://github.com/poniponiponiponiponiponiponiponiponi/stuff ~/Projects/stuff

if ! command -v paru; then
    (cd ~/FOSS/paru && makepkg -si)
fi

paru -S imhex-bin

#curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
#source "$HOME/.cargo/env"
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

git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.st status

sudo localectl set-x11-keymap pl
sudo localectl set-x11-keymap "" "" "" ctrl:nocaps

if ! grep -q FONT /etc/vconsole.conf; then
    if [ -e "/usr/share/kbd/consolefonts/eurlatgr.psfu.gz" ]; then
        echo "FONT=eurlatgr" >> /etc/vconsole.conf
    else
        echo "consolefont not found!"
    fi
fi

if ! grep -q "disassembly-flavor" ~/.gdbinit; then
    echo "set disassembly-flavor att" >> ~/.gdbinit
else
    echo "disassembly-flavor is already set"
fi

sudo echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf
sudo echo "blacklist snd_pcsp" >> /etc/modprobe.d/nobeep.conf
