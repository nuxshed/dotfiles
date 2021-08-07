local colors = require("colors." .. O.colorscheme).colors

require("nvim-web-devicons").setup {
  override = {
    html = {
      icon = "",
      color = colors.red,
      name = "html",
    },
    css = {
      icon = "",
      color = colors.blue,
      name = "css",
    },
    js = {
      icon = "",
      color = colors.yellow,
      name = "js",
    },
    ts = {
      icon = "ﯤ",
      color = colors.blue,
      name = "ts",
    },
    png = {
      icon = "",
      color = colors.dark_purple,
      name = "png",
    },
    jpg = {
      icon = "",
      color = colors.dark_purple,
      name = "jpg",
    },
    jpeg = {
      icon = "",
      color = "colors.dark_purple",
      name = "jpeg",
    },
    Dockerfile = {
      icon = "",
      color = colors.cyan,
      name = "Dockerfile",
    },
    rb = {
      icon = "",
      color = colors.red,
      name = "rb",
    },
    vue = {
      icon = "﵂",
      color = colors.green,
      name = "vue",
    },
    py = {
      icon = "",
      color = colors.cyan,
      name = "py",
    },
    rs = {
      icon = "",
      color = colors.orange,
      name = "rs",
    },
    toml = {
      icon = "",
      color = colors.orange,
      name = "toml",
    },
    lock = {
      icon = "",
      color = colors.light_grey,
      name = "lock",
    },
  },
}
