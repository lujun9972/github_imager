# github-imager.el

将本地文件引用转换为 GitHub raw 内容 URL 的 Emacs 工具包。

在 Emacs 中撰写博客或文档时，如果引用了存放在 Git 仓库中的图片或其他资源，往往需要把相对路径转换为可公开访问的 GitHub URL。本工具包可以自动完成这项工作——通过 `github-convert-link` 处理单个文件，或通过 `github-convert-body` 批量替换整个文本中的 HTML/Markdown 链接。

## 功能特性

- **单文件转换** — `github-convert-link` 接受本地文件路径，根据当前 Git 远程仓库和分支返回对应的 `raw.githubusercontent.com` URL。
- **批量文本转换** — `github-convert-body` 扫描文本中的相对链接（HTML 的 `src`/`href` 属性、Markdown 的 `[][id]` 引用），就地替换为 GitHub raw URL。
- **可插拔的链接规则** — `github-convert-rules` 是一个可自定义的 alist，由 `(REGEXP . GROUP)` 对组成，你可以添加对 Org-mode 链接或其他格式的支持。
- **安全检查** — 自动跳过绝对 URI（http、https 等），当引用的本地文件不存在时会发出警告。

## 快速开始

### 安装

直接加载文件：

```elisp
(load "/path/to/github-convert.el")
```

或使用 `use-package` + `straight.el`：

```elisp
(use-package github-convert
  :straight (github-convert :repo "lujun9972/github_imager" :host github))
```

### 使用方法

**转换单个文件路径：**

`M-x github-convert-link` — 提示选择文件，将 GitHub raw URL 复制到 kill ring。

**转换字符串中所有相对链接：**

```elisp
(github-convert-body "[img][logo]" "/path/to/article.org")
;; => "[https://raw.githubusercontent.com/user/repo/main/images/logo.png]"
```

## 配置

### `github-convert-rules`

`(REGEXP . GROUP)` 对组成的 alist，定义文本中哪些部分被视为本地文件引用。默认值：

```elisp
'(("<[a-zA-Z]+[^/>]+\\(src\\|href\\)=\"\\([^#\"][^\"]*\\)\"[^>]*>" . 2) ; HTML 链接
  ("\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]" . 2))                         ; Markdown 链接
```

例如，添加一条 Org-mode 图片链接的规则：

```elisp
(add-to-list 'github-convert-rules '("\\[\\[\\([^]]+\\)\\]" . 1))
```

## 许可证

本项目目前未包含许可证文件。如需了解使用条款，请联系作者。

## 作者

darksun (<lujun9972@gmail.com>)
