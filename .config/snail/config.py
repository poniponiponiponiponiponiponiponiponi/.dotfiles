# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# define timeout for multipurpose_modmap
define_timeout(1)

# [Global modemap] Change modifier keys as in xmodmap
define_modmap({
    Key.CAPSLOCK: Key.LEFT_CTRL
})

# [Conditional modmap] Change modifier keys in certain applications
define_conditional_modmap(re.compile(r'Emacs'), {
    Key.RIGHT_CTRL: Key.ESC,
})

# Keybindings for Firefox/Chrome
define_keymap(re.compile("firefox"), {
    # mine!
    K("C-s"): K("C-f")
}, "Firefox and Chrome")

# Emacs-like keybindings in non-Emacs applications
define_keymap(lambda wm_class: wm_class not in ("Emacs", "Alacritty"), {
    K("M-backspace"): K("C-backspace"),
    # Cursor
    K("C-b"): K("left"),
    K("C-Shift-b"): K("Shift-left"),
    K("C-f"): K("right"),
    K("C-Shift-f"): K("Shift-right"),
    K("C-p"): K("up"),
    K("C-n"): K("down"),
    # Forward/Backward word
    K("M-b"): K("C-left"),
    K("M-Shift-b"): K("C-Shift-left"),
    K("M-f"): K("C-right"),
    K("M-Shift-f"): K("C-Shift-right"),
}, "Emacs-like keys")
