#!/bin/bash

DOTFILES_PATH="$HOME/.dotfiles"

if [[ "`uname -a`" == *"fedora"* ]]; then
    sudo dnf copr enable gourlaysama/dust
    sudo dnf install -y \
        https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
        https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
    sudo dnf update -y
    sudo dnf install -y rofi neovim wine xclip gcc exa ripgrep dust fd-find zsh \
        alacritty tmux mpv ipython python3 gdb make cmake g++ dejavu-fonts-all \
        ruby gem java-17-openjdk-devel java-17-openjdk clang-tools-extra \
        xz-devel openssl-devel fontawesome-fonts acpi i3blocks feh dunst \
        deluge-gtk rust-analyzer lxappearance htop curl wget bat pip patchelf \
        qemu-user qemu-user-static gcc-riscv64-linux-gnu \
        binutils-riscv64-linux-gnu gcc-aarch64-linux-gnu \
        binutils-aarch64-linux-gnu \
        texinfo readline-devel sqlite3 arc-theme unrar gimp blender light \
        p7zip p7zip-plugins strace ltrace ruby-devel binwalk \
        java-runtime-decompiler nasm perf

    # codecs
    sudo dnf install -y gstreamer1-plugins-{bad-\*,good-\*,base} \
        gstreamer1-plugin-openh264 gstreamer1-libav \
        --exclude=gstreamer1-plugins-bad-free-devel
    sudo dnf install -y lame\* --exclude=lame-devel
    sudo dnf group upgrade -y --with-optional Multimedia --allowerasing
fi


if [[ "`uname -a`" == *"arch"* ]]; then
	sudo pacman -Syu emacs firefox neovim ttf-dejavu \
		noto-fonts noto-fonts-cjk noto-fonts-emoji \
		gcc python python-pip zip p7zip wget git curl \
		openbsd-netcat ipython ruby rubygems mpv tmux fish ropper \
		python-pwntools clang htop bat patchelf perf nasm binwalk \
		blender ltrace strace unrar rust-analyzer dunst feh acpi \
		ttf-font-awesome lxappearance wine-staging xclip exa ripgrep dust \
		zsh alacritty gdb make cmake bear bash-completion man \
		qemu-full jdk-openjdk openjdk-doc openjdk-src \
		riscv32-elf-binutils riscv32-elf-gdb riscv32-elf-newlib \
		riscv64-elf-binutils riscv64-elf-gcc riscv64-elf-gdb \
		riscv64-elf-newlib riscv64-linux-gnu-binutils \
		riscv64-linux-gnu-gcc riscv64-linux-gnu-gdb \
		riscv64-linux-gnu-glibc riscv64-linux-gnu-linux-api-headers \
		aarch64-linux-gnu-binutils aarch64-linux-gnu-gcc \
		aarch64-linux-gnu-gdb aarch64-linux-gnu-glibc \
		arm-none-eabi-binutils arm-none-eabi-gcc arm-none-eabi-gdb \
		arm-none-eabi-newlib python-ipdb inet-utils
fi

declare -a commands=("ipython" "git" "gem" "pip" "python" "curl")
for command in "${commands[@]}"
do
    if ! command -v "$command" &> /dev/null
    then
        echo "NO ${command} DETECTED. Please make sure you have everything" \
            "installed before proceeding."
        exit 1
    fi
done

mkdir -p ~/.config
mkdir -p ~/.config/mpv
mkdir -p ~/.config/tmux
mkdir -p ~/.config/i3
mkdir -p ~/Projects
mkdir -p ~/FOSS
mkdir -p ~/.scripts

#pip install --upgrade pip
#pip install pwntools
#pip install pyright
#pip install ropper
#pip install ipdb
sudo gem install one_gadget
sudo gem install seccomp-tools

mkdir -p ~/FOSS/pwndbg
git clone https://github.com/pwndbg/pwndbg ~/FOSS/pwndbg
~/FOSS/pwndbg/setup.sh


ln -sf "$DOTFILES_PATH/mpv.conf" ~/.config/mpv/mpv.conf
ln -sf "$DOTFILES_PATH/pwninit_template.py" ~/.config/pwninit_template.py
ln -sf "$DOTFILES_PATH/tmux.conf" ~/.config/tmux/tmux.conf
ln -sf "$DOTFILES_PATH/alacritty.yml" ~/.config/alacritty.yml
ln -sf "$DOTFILES_PATH/.emacs" ~/.emacs
ln -sf "$DOTFILES_PATH/.bashrc" ~/.bashrc
ln -sf "$DOTFILES_PATH/sway_config" ~/.config/i3/config
ln -sf "$DOTFILES_PATH/picom.conf" ~/.config/picom.conf

mkdir -p ~/FOSS/sig-database
git clone https://github.com/push0ebp/sig-database ~/FOSS/sig-database

#curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
#source "$HOME/.cargo/env"
#rustup component add rustfmt
#rustup component add clippy

git config --global pager.branch false
git config --global user.name "tabun-dareka"
git config --global user.email "tabun.dareka@protonmail.com"
git config --global credential.helper store
