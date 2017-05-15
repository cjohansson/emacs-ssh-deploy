# `emacs-ssh-deploy` [![MELPA](http://melpa.org/packages/ssh-deploy-badge.svg)](http://melpa.org/#/ssh-deploy) [![MELPA Stable](http://stable.melpa.org/packages/ssh-deploy-badge.svg)](http://stable.melpa.org/#/ssh-deploy)

The `ssh-deploy` plug-in for Emacs makes it possible to effortlessly deploy local files and directories to remote hosts via SSH and FTP. It also makes it possible to define remote paths per directory and whether or not you want to deploy on explicit save actions or not and whether you want transfers to be asynchronous or not. For asynchronous transfers you need a setup which doesn't require a interactive authorization. The plug-in also enables manual upload and download of files and directories. It also features automatic detection of remote changes. You can also check differences between local files and remote files if you have `ediff` installed. You can rename and delete files and directories synced to remote host. You can also browse remote hosts. Lastly you can easily open remote hosts terminal if you have `tramp-term` installed. For asynchronous transfers **you need to have a setup which allows automatic connections to servers via SSH and FTP and have async.el installed**.

`ssh-deploy` works with `DirectoryVariables` so you can have different deploy setups in different ways for different folders.

The idea for this plug-in was to mimic the behavior of **PhpStorm** deployment functionality.

This application is made by Christian Johansson <christian@cvj.se> 2016 and is licensed under GNU General Public License 3.


## A setup example

* Download ssh-deploy and place it at `~/.emacs.d/ssh-deploy/` or install via `package.el` (`M-x list-packages`) from the `MELPA` repository.
* So if you want to deploy `/Users/username/Web/MySite/` to create this `DirectoryVariables` file in your project root at `/Users/username/Web/MySite/.dir-locals.el`.

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com:/var/www/MySite/")
  (ssh-deploy-on-explicit-save . t)
)))
```
For automatic **SSH** connections you need to setup a password-less public-key authorization.
Or for FTP use this:

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ftp:myuser@myserver.com:/MySite/")
  (ssh-deploy-on-explicit-save . t)
)))
```
For automatic **FTP** connections you need to setup `~/.netrc` with your login credentials. An example:
`~/.netrc` contents:

``` shell
machine myserver.com login myuser port ftp password mypassword
```
Set your user and group as owner and file permissions to `700`. Emacs should now be able to automatically connect to this server via FTP without any user interaction.

* And add this to your *emacs-init-script*:

``` elisp
;; ssh-deploy - prefix = C-c C-z, f = forced upload, u = upload, d = download, x = diff, t = terminal, b = browse
(add-to-list 'load-path "~/.emacs.d/ssh-deploy/")
(use-package ssh-deploy
  :config
  (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))
  (add-hook 'find-file-hook (lambda() (if ssh-deploy-automatically-detect-remote-changes (ssh-deploy-remote-changes-handler)) ))
  (global-set-key (kbd "C-c C-z f") (lambda() (interactive)(ssh-deploy-upload-handler-forced) ))
  (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
  (global-set-key (kbd "C-c C-z D") (lambda() (interactive)(ssh-deploy-delete-handler) ))
  (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
  (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
  (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-handler) ))
  (global-set-key (kbd "C-c C-z r") (lambda() (interactive)(ssh-deploy-rename-handler) ))
  (global-set-key (kbd "C-c C-z e") (lambda() (interactive)(ssh-deploy-remote-changes-handler) ))
  (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-handler) )))
```

You can remove the `add-to-list` line if you installed via `MELPA` repository.

* Restart Emacs

* Now when you save a file somewhere under the directory `/Users/username/Web/MySite/`, the script will launch and deploy the file with the remote server.
* If you press `C-c C-z x` and the current buffer is a file, you will launch a `ediff` session showing differences between local file and remote file via `tramp`, or if current buffer is a directory it will show a error that directories are not yet supported for differences.
* If you press `C-c C-z f` you will **force** upload local file or directory to remote host even if they have external changes.
* If you press `C-c C-z u` you will upload local file or directory to remote host.
* If you press `C-c C-z d` you will download the current file or directory from remote host and then reload current buffer.
* If you press `C-c C-z D` you will delete the current file or directory after a confirmation on local and remote host.
* If you press `C-c C-z t` you will open a terminal with remote host via `tramp-term`.
* If you press `C-c C-z b` you will browse current directory on remote host in `dired-mode`.
* If you press `C-c C-z r` you will rename current file or directory.
* If you press `C-c C-z e` you will check for remote changes to the current file.

The local path and local root is evaluated based on their **truename** so if you use different symbolic local paths it shouldn't affect the deployment procedure.

The above configuration example uses the Emacs plug-in `use-package` which I highly recommend.

## More complex SSH connections

If you have a SSH connection that is using a different identity-file than the default, or if it is using a different port than the default you just need to edit your local SSH-config `~/ssh/config` to make it work using this plugin, like this:

``` bash
## My special connection (replace remote-host, remote-port and identity-file with your values)
Host remote-host
    Port remote-port
    IdentityFile identity-file
```

## Read more
* <https://www.emacswiki.org/emacs/DirectoryVariables>
* <http://www.gnu.org/software/tramp/>
* <https://github.com/jwiegley/use-package>
* <https://www.emacswiki.org/emacs/EdiffMode>
* <http://melpa.org/>
* <https://github.com/randymorris/tramp-term.el>
* <https://github.com/jwiegley/emacs-async>
