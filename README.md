# pelpa-mode
pelpa-mode是用来监控popkit elpa源的运行情况

## 安装
将pelpa-mode.el文件拷贝到一个目录下（如~/github/pelpa-mode/pelpa-mode.el）  
```elisp
(load-file "~/github/pelpa-mode/pelpa-mode.el")
(require 'pelpa-mode)
```

## 使用
执行**M-x pm/monitor**然后会显示出一个\*pelpa*的buffer

## 快捷键
. **r**  刷新状态
