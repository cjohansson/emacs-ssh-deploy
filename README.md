# `emacs-ssh-deploy` [![MELPA](http://melpa.org/packages/ssh-deploy-badge.svg)](http://melpa.org/#/ssh-deploy) [![MELPA Stable](http://stable.melpa.org/packages/ssh-deploy-badge.svg)](http://stable.melpa.org/#/ssh-deploy)

The `ssh-deploy` plug-in for Emacs makes it possible to effortlessly deploy local files and directories to remote hosts via TRAMP (including but not limited to SSH, SFTP, FTP). It tries to provide functions that can be easily used by custom scripts.

## Features:
* Define syncing configuration per directory or per file (using `DirectoryVariables` or `File Variables`)
* Control whether uploads of files should be automatic on save
* Manual downloads and uploads of directories and files
* Automatic and manual detection of remote changes of files using local revisions
* Launch remote `eshell` terminals in base or relative directory
* Launch remote `dired` browsing in base or relative directory
* Launch difference sessions for files using `ediff`
* Launch difference sessions for directories using a custom implementation of recursive directory differences over TRAMP based on `ediff`
* Rename files and directories on local host and have it mirrored on the remote
* Delete files and directories on local host and have it mirrored on the remote
* Open corresponding file on the remote host
* Open SQL database-session on remote hosts
* All operations support asynchronous mode if `async.el` is installed. (You need to setup an automatic authorization for this, i.e. `~/.netrc`, `~/.authinfo` or `~/.authinfo.gpg` and/or key-based password-less authorization)

The idea for this plug-in was to mimic the behavior of **PhpStorm** deployment functionality.

This application is made by Christian Johansson <christian@cvj.se> 2016-2018 and is licensed under GNU General Public License 3 (GNU GPL 3).

## Configuration

Here is a list of other variables you can set globally or per directory:

* `ssh-deploy-root-local` The local root that should be under deployment *(string)*
* `ssh-deploy-root-remote` The remote TRAMP root that is used for deployment *(string)*
* `ssh-deploy-debug` Enables debugging messages *(boolean)*
* `ssh-deploy-revision-folder` The folder used for storing local revisions *(string)*
* `ssh-deploy-automatically-detect-remote-changes` Enables automatic detection of remote changes *(boolean)*
* `ssh-deploy-on-explicit-save` Enabled automatic uploads on save *(boolean)*
* `ssh-deploy-exclude-list` A list defining what paths to exclude from deployment *(list)*
* `ssh-deploy-async` Enables asynchronous transfers (you need to have `async.el` installed as well) *(boolean)*
* `ssh-deploy-remote-sql-database` Default database when connecting to remote SQL database *(string)*
* `ssh-deploy-remote-sql-password` Default password when connecting to remote SQL database *(string)*
* `ssh-deploy-remote-sql-server` Default server when connecting to remote SQL database *(string)*
* `ssh-deploy-remote-sql-user` Default user when connecting to remote SQL database *(string)*


## Deployment configuration examples

* Download ssh-deploy and place it at `~/.emacs.d/ssh-deploy/` or install via `package.el` (`M-x list-packages` or `M-x package-install` + `ssh-deploy`) from the `MELPA` repository.
* So if you want to deploy `/Users/username/Web/MySite/` to create this `DirectoryVariables` file in your project root at `/Users/username/Web/MySite/.dir-locals.el`.

You really need to do a bit of research about how to connect via different protocols using TRAMP on your operating system, I think Windows users should use `plink` for most protocols. Linux should work out of the box and macOS requires a bit of tweaking to get FTP support.

### SSH/SFTP with SQL

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com:/var/www/MySite/")
  (ssh-deploy-on-explicit-save . t)
  (ssh-deploy-remote-sql-database . "myuser")
  (ssh-deploy-remote-sql-password . "mypassword")
  (ssh-deploy-remote-sql-server . "myserver")
  (ssh-deploy-remote-sql-user . "myuser")
)))
```

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/sftp:myuser@myserver.com:/var/www/MySite/")
  (ssh-deploy-on-explicit-save . t)
)))
```

You can pipe remote connections as well like this:

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com|sudo:web@myserver.com:/var/www/MySite/")
  (ssh-deploy-async . nil)
  (ssh-deploy-on-explicit-save . t)
)))
```

If you have a password-less sudo on your remote host you should be to do this asynchronously.

### FTP

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ftp:myuser@myserver.com:/MySite/")
  (ssh-deploy-on-explicit-save . t)
)))
```

## Interaction-free SSH setup using password-less public-key authorization

For automatic **SSH** connections you need to setup a password-less public-key authorization. You need to research how to setup this on your operating system.

## Changing SSH port or public-key identify-file per host

If you have a SSH connection that is using a different identity-file than the default, or if it is using a different port than the default you just need to edit your local SSH-config `~/.ssh/config` to make it work using this plug-in, like this:

``` bash
## My special connection (replace remote-host, remote-port and identity-file with your values)
Host remote-host
    Port remote-port
    IdentityFile identity-file
```

## Interaction-free password-based setup on *NIX systems

For automatic **FTP** connections you need to setup `~/.netrc`, `~/.authinfo` or `~/.authinfo.gpg` with your login credentials. An example of contents:

``` shell
machine myserver.com login myuser port ftp password mypassword
machine myserver2.com login myuser2 port ssh password mypassword2
machine myserver3.com login myuser3 port sftp password mypassword3
```

Set your user and group as owner and file permissions to `600`. Emacs should now be able to automatically connect to this server via FTP without any user interaction.

## Interaction-free SSH setup using public-key password-based authorization

By combining a `~/.netrc`, `~/.authinfo` or `~/.authinfo.gpg` setup and a `public-key` setup you should be able to have a interaction-free public-key password-based authorization that can be used asynchronously.

## Emacs configuration example

* And add this to your *emacs-init-script*: (1)

``` elisp
;; ssh-deploy - prefix = C-c C-z, f = forced upload, u = upload, d = download, x = diff, t = terminal, b = browse
(add-to-list 'load-path "~/.emacs.d/ssh-deploy/")
(require 'ssh-deploy)
(add-hook 'after-save-hook (lambda() (if (and (boundp 'ssh-deploy-on-explicit-save) ssh-deploy-on-explicit-save) (ssh-deploy-upload-handler)) ))
(add-hook 'find-file-hook (lambda() (if (and (boundp 'ssh-deploy-automatically-detect-remote-changes) ssh-deploy-automatically-detect-remote-changes) (ssh-deploy-remote-changes-handler)) ))
(global-set-key (kbd "C-c C-z f") (lambda() (interactive)(ssh-deploy-upload-handler-forced) ))
(global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
(global-set-key (kbd "C-c C-z D") (lambda() (interactive)(ssh-deploy-delete-handler) ))
(global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
(global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
(global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-eshell-base-handler) ))
(global-set-key (kbd "C-c C-z T") (lambda() (interactive)(ssh-deploy-remote-terminal-eshell-handler) ))
(global-set-key (kbd "C-c C-z R") (lambda() (interactive)(ssh-deploy-rename-handler) ))
(global-set-key (kbd "C-c C-z e") (lambda() (interactive)(ssh-deploy-remote-changes-handler) ))
(global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-base-handler) ))
(global-set-key (kbd "C-c C-z o") (lambda() (interactive)(ssh-deploy-open-remote-file-handler) ))
(global-set-key (kbd "C-c C-z m") (lambda() (interactive)(ssh-deploy-remote-sql-mysql-handler) ))
```

* Or use the `use-package` and `hydra-script` I'm using:

``` elisp
      (use-package ssh-deploy
        :ensure t
        :demand
        :bind (("C-c C-z" . hydra-ssh-deploy/body))
        :hook ((after-save . (lambda() (if (and (boundp 'ssh-deploy-on-explicit-save) ssh-deploy-on-explicit-save) (ssh-deploy-upload-handler)) ))
               (find-file . (lambda() (if (and (boundp 'ssh-deploy-automatically-detect-remote-changes) ssh-deploy-automatically-detect-remote-changes) (ssh-deploy-remote-changes-handler)) )))
        :config
        (defhydra hydra-ssh-deploy (:color red :hint nil)
          "
    _u_: Upload                              _f_: Force Upload
    _d_: Download
    _D_: Delete
    _x_: Difference
    _t_: Eshell Base Terminal                _T_: Eshell Relative Terminal
    _e_: Detect Remote Changes
    _R_: Rename
    _b_: Browse Base                         _B_: Browse Relative
    _o_: Open current file on remote         _m_: Open sql-mysql on remote
    "
          ("f" ssh-deploy-upload-handler-forced)
          ("u" ssh-deploy-upload-handler)
          ("d" ssh-deploy-download-handler)
          ("D" ssh-deploy-delete-handler)
          ("x" ssh-deploy-diff-handler)
          ("t" ssh-deploy-remote-terminal-eshell-base-handler)
          ("T" ssh-deploy-remote-terminal-eshell-handler)
          ("e" ssh-deploy-remote-changes-handler)
          ("R" ssh-deploy-rename-handler)
          ("b" ssh-deploy-browse-remote-base-handler)
          ("B" ssh-deploy-browse-remote-handler)
          ("o" ssh-deploy-open-remote-file-handler)
          ("m" ssh-deploy-remote-sql-mysql-handler)))
```

(1) You can remove the `(add-to-list)` and `(require)` lines if you installed via `MELPA` repository.

* Restart Emacs or re-evaluate your *emacs-init-script*

## Example usage

* Now when you save a file somewhere under the directory `/Users/username/Web/MySite/`, the script will launch and deploy the file with the remote server.
* If you press `C-c C-z x` and the current buffer is a file, you will launch a `ediff` session showing differences between local file and remote file via TRAMP, or if current buffer is a directory it will open a buffer showing directory differences
* If you press `C-c C-z f` you will **force** upload local file or directory to remote host even if they have external changes.
* If you press `C-c C-z u` you will upload local file or directory to remote host.
* If you press `C-c C-z d` you will download the current file or directory from remote host and then reload current buffer.
* If you press `C-c C-z D` you will delete the current file or directory after a confirmation on local and remote host.
* If you press `C-c C-z t` you will open a terminal with remote host in base directory via `eshell`.
* If you press `C-c C-z T` you will open a terminal with remote host in current directory via `eshell`.
* If you press `C-c C-z b` you will browse base directory on remote host in `dired`.
* If you press `C-c C-z B` you will browse current directory on remote host in `dired`.
* If you press `C-c C-z R` you will rename current file or directory.
* If you press `C-c C-z e` you will check for remote changes to the current file.
* If you press `C-c C-z o` you will open remote file corresponding to local file.
* If you press `C-c C-z m` you will open remote sql-mysql session on remote host.

The local path and local root is evaluated based on their `truename` so if you use different symbolic local paths it shouldn't affect the deployment procedure.

The above configuration example uses the Emacs plug-in `use-package` which I highly recommend.

## TRAMP FTP problem in macOS 10.13

macOS 10.13 removed the Darwin port of BSD `ftp` which is needed for `ange-ftp`, which is required by TRAMP. You can get it back by doing this:

1. Download <https://opensource.apple.com/tarballs/lukemftp/lukemftp-16.tar.gz> or some other version from <https://opensource.apple.com/tarballs/lukemftp/>
2. Extract archive
3. Visit folder for `tnftp` inside the extracted archive in terminal
4. Type `./configure` then `make` and then `sudo make install`
5. Type `mv ./src/ftp /usr/local/bin/ftp`

## Read more
* <http://www.gnu.org/software/tramp/>
* <http://melpa.org/>
* <https://www.emacswiki.org/emacs/DirectoryVariables>
* <https://www.emacswiki.org/emacs/EdiffMode>
* <https://github.com/jwiegley/emacs-async>
* <https://github.com/jwiegley/use-package>
