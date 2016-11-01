# `ssh-deploy`

The `ssh-deploy` plug-in makes it possible to effortlessly deploy local files and directories to remote hosts via SSH and FTP. It also makes it possible to define remote paths per directory and whether or not you want to deploy on explicit save actions or not. Also it enables manual upload and download of files and directories. You can also check differences between local files and directories and remote files and directories if you have `tramp`,`ediff` and `ztree` installed. You can also browse remote hosts if you have `tramp` installed. Lastly you can easily open remote hosts terminal if you have `tramp-term` installed. **You need to have a setup which allows password-less key-based logins to servers via SSH and have curl and scp installed locally**.

`ssh-deploy` works with `DirectoryVariables` so you can have different deploy setups in different ways for different folders.

The idea for this plug-in was to mimic the behavior of **PhpStorm** deployment functionality.

This application is made by Christian Johansson <christian@cvj.se> 2016 and is licensed under GNU General Public License 3.


## A setup example

* Download ssh-deploy and place it at `~/.emacs.d/ssh-deploy/` or install via `package.el` from the `MELPA` repository.

* Create this `DirectoryVariables` file in your project root at `/Users/username/Web/MySite/.dir-locals.el`.

``` emacs-lisp
((nil . (
(ssh-deploy-root-local . "/Users/username/Web/MySite/")
(ssh-deploy-root-remote . "user@myserver.com:/var/www/MySite/")
(ssh-deploy-on-explicit-save . t)
)))
```

Or for FTP use this

``` emacs-lisp
((nil . (
(ssh-deploy-root-local . "/Users/username/Web/MySite/")
(ssh-deploy-root-remote . "/ftp:user:password@myserver.com:/MySite/")
(ssh-deploy-on-explicit-save . t)
)))

```


* And add this to your *emacs-init-script*:

``` elisp
;; ssh-deploy - prefix = C-c C-z, u = upload, d = download, x = diff, t = terminal, b = browse
(add-to-list 'load-path "~/.emacs.d/ssh-deploy/")
(use-package ssh-deploy
  :config
  (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))
  (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
  (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
  (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
  (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-handler) ))
  (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-handler) )))
```

You can remove the `add-to-list` line if you installed via `MELPA` repository.

* Now when you save a file somewhere under the directory `/Users/username/Web/MySite/`, the script will launch and deploy the file with the remote server.
* If you press `C-c C-z x` and the current buffer is a file, you will launch a `ediff` session showing differences between local file and remote file via `tramp`, or if current buffer is a directory it will show differences with remote directory using `ztree-diff` via `tramp`.
* If you press `C-c C-z u` you will upload local file or directory to remote host.
* If you press `C-c C-z d` you will download the current file or directory from remote host and then reload current buffer.
* If you press `C-c C-z t` you will open a terminal with remote host via `tramp-term`.
* If you press `C-c C-z b` you will browse current directory on remote host in `dired-mode`.

The local path and local root is evaluated based on their **truename** so if you use different symbolic local paths it shouldn't affect the deployment procedure.

The above configuration uses the plugin `use-package` which I highly recommend.

## More complex SSH connections

If you have a SSH connection that is using a different identity-file than the default, or if it is using a different port than the default you just need to edit your local SSH-config (~/ssh/config) to make it work using this plugin, like this:

``` bash

## My special connection (replace remote-host, port and identity-file with your values)
Host remote-host
    Port port
    IdentityFile identity-file

```

## Read more
* <https://www.emacswiki.org/emacs/DirectoryVariables>
* <http://www.gnu.org/software/tramp/>
* <https://github.com/jwiegley/use-package>
* <https://www.emacswiki.org/emacs/EdiffMode>
* <http://melpa.org/>
* <https://github.com/fourier/ztree>
* <https://github.com/randymorris/tramp-term.el>
