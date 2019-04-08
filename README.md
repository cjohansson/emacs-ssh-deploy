# `emacs-ssh-deploy`
[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt) [![MELPA](https://melpa.org/packages/ssh-deploy-badge.svg)](https://melpa.org/#/ssh-deploy) [![MELPA Stable](https://stable.melpa.org/packages/ssh-deploy-badge.svg)](https://stable.melpa.org/#/ssh-deploy)

The `ssh-deploy` plug-in for Emacs makes it possible to effortlessly deploy local files and directories to remote hosts via Tramp (including but not limited to SSH, SFTP, FTP). It tries to provide functions that can be easily used by custom scripts.

## Features:

* Define syncing configuration per directory or per file (using `DirectoryVariables` or `File Variables`)
* Control whether uploads of files should be automatic on save
* Manual downloads and uploads of directories and files
* Automatic and manual detection of remote changes of files using local revisions
* Launch remote `eshell` and `shell` terminals in base or relative directory
* Launch remote `dired` browsing in base or relative directory
* Launch difference sessions for files using `ediff`
* Launch difference sessions for directories using a custom implementation of recursive directory differences over Tramp based on `ediff`
* Rename files and directories on local host and have it mirrored on the remote
* Delete files and directories on local host and have it mirrored on the remote
* Open corresponding file on the remote host
* Open SQL database-session on remote hosts
* Run custom deployment scripts
* All operations support asynchronous mode if `(make-thread`) or `async.el` is installed. (You need to setup an automatic authorization for this, i.e. `~/.authinfo.gpg` and/or key-based password-less authorization)
* Tries to follow best-practice both in terms of performance and code style

The idea for this plug-in was to mimic the behavior of **PhpStorm** deployment functionality.

This application is made by Christian Johansson <christian@cvj.se> 2016-2018 and is licensed under GNU General Public License 3 (GNU GPL 3).

## Configuration

Here is a list of other variables you can set globally or per directory:

* `ssh-deploy-root-local` The local root that should be under deployment *(string)*
* `ssh-deploy-root-remote` The remote Tramp root that is used for deployment *(string)*
* `ssh-deploy-debug` Enables debugging messages *(integer)*
* `ssh-deploy-revision-folder` The folder used for storing local revisions *(string)*
* `ssh-deploy-automatically-detect-remote-changes` Enables automatic detection of remote changes *(integer)*
* `ssh-deploy-on-explicit-save` Enabled automatic uploads on save *(integer)*
* `ssh-deploy-exclude-list` A list defining what paths to exclude from deployment *(list)*
* `ssh-deploy-async` Enables asynchronous transfers (you need to have `(make-thread)` or `async.el` installed as well) *(integer)*
* `ssh-deploy-remote-sql-database` Default database when connecting to remote SQL database *(string)*
* `ssh-deploy-remote-sql-password` Default password when connecting to remote SQL database *(string)*
* `ssh-deploy-remote-sql-port` - Default port when connecting to remote SQL database *(integer)*
* `ssh-deploy-remote-sql-server` Default server when connecting to remote SQL database *(string)*
* `ssh-deploy-remote-sql-user` Default user when connecting to remote SQL database *(string)*
* `ssh-deploy-remote-shell-executable` Default remote shell executable when launching shell on remote host *(string)*
* `ssh-deploy-verbose` Show messages in message buffer when starting and ending actions *(integer)*
* `ssh-deploy-script` - Your custom lambda function that will be called using (funcall) when running deploy script handler *(function)*
* `ssh-deploy-async-with-threads` - Whether to use threads (make threads) instead of processes (async-start) for asynchronous operations *(integer)*

When integers are used as booleans, above zero means true, zero means false and nil means unset and fallback to global settings.

## Deployment configuration examples

* Download ssh-deploy and place it at `~/.emacs.d/ssh-deploy/` or install via `package.el` (`M-x list-packages` or `M-x package-install` + `ssh-deploy`) from the `ELPA` or `MELPA` repository.
* So if you want to deploy `/Users/username/Web/MySite/` to create this `DirectoryVariables` file in your project root at `/Users/username/Web/MySite/.dir-locals.el`.

You really need to do a bit of research about how to connect via different protocols using Tramp on your operating system, I think Windows users should use `plink` for most protocols. Linux should work out of the box and macOS requires a bit of tweaking to get FTP support.

### SSH, with automatic uploads and SQL

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com:/var/www/MySite/")
  (ssh-deploy-on-explicit-save . 1)
  (ssh-deploy-remote-sql-database . "myuser")
  (ssh-deploy-remote-sql-password . "mypassword")
  (ssh-deploy-remote-sql-user . "myuser")
)))
```

### SFTP, with automatic uploads

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/sftp:myuser@myserver.com:/var/www/MySite/")
  (ssh-deploy-on-explicit-save . 1)
)))
```

### SSH, custom port 2120, not asynchronous and without automatic uploads

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com#2120:/var/www/MySite/")
  (ssh-deploy-on-explicit-save . 0)
  (ssh-deploy-async . 0)
)))
```

You can pipe remote connections as well like this:

### SSH, asynchronous using threads, with automatic uploads, piped to other user on remote server and with custom deployment script.

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com|sudo:web@myserver.com:/var/www/MySite/")
  (ssh-deploy-async . 1)
  (ssh-deploy-async-with-threads . 1)
  (ssh-deploy-on-explicit-save . 1)
  (ssh-deploy-script . (lambda() (let ((default-directory ssh-deploy-root-remote)) (shell-command "bash compile.sh"))))
)))
```

### SSH, asynchronous not using threads, without automatic uploads, piped to other user on remote server and with custom deployment script.

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com|sudo:web@myserver.com:/var/www/MySite/")
  (ssh-deploy-async . 1)
  (ssh-deploy-async-with-threads . 0)
  (ssh-deploy-on-explicit-save . 0)
  (ssh-deploy-script . (lambda() (let ((default-directory ssh-deploy-root-local)) (shell-command "bash compile.sh") (ssh-deploy-upload-handler))))
)))
```

If you have a password-less sudo on your remote host you should be to do this asynchronously or if you have your sudo credentials in your `~/.authinfo.gpg` file.

### FTP, with automatic uploads

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ftp:myuser@myserver.com:/MySite/")
  (ssh-deploy-on-explicit-save . 1)
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

For automatic **FTP** connections you need to setup `~/.authinfo.gpg` with your login credentials. An example of contents:

``` shell
machine myserver.com login myuser port ftp password mypassword
machine myserver2.com login myuser2 port ssh password mypassword2
machine myserver3.com login myuser3 port sftp password mypassword3
```

Set your user and group as owner and file permissions to `600`. Emacs should now be able to automatically connect to this server via FTP without any user interaction.

## Interaction-free SSH setup using public-key password-based authorization

By combining a `~/.authinfo.gpg` setup and a `public-key` setup you should be able to have a interaction-free public-key password-based authorization that can be used asynchronously.

## Emacs configuration example

* And add this to your *emacs-init-script*: (1)

``` elisp
;; ssh-deploy - prefix = C-c C-z, f = forced upload, u = upload, d = download, x = diff, t = terminal, b = browse, h = shell
(add-to-list 'load-path "~/.emacs.d/ssh-deploy/")
(require 'ssh-deploy)
(ssh-deploy-line-mode) ;; If you want mode-line feature
(ssh-deploy-add-menu) ;; If you want menu-bar feature
(ssh-deploy-add-after-save-hook) ;; If you want automatic upload support
(ssh-deploy-add-find-file-hook) ;; If you want detecting remote changes support
(global-set-key (kbd "C-c C-z") 'ssh-deploy-prefix-map)
```

If you want to use the pre-defined hydra you can use this key-binding instead:
``` elisp
(global-set-key (kbd "C-c C-z") 'ssh-deploy-hydra/body)
```

* Or use the `use-package` and `hydra-script` I'm using:

``` elisp
      (use-package ssh-deploy
        :ensure t
        :demand
        :after hydra
        :bind (("C-c C-z" . ssh-deploy-hydra/body))
        :hook ((after-save . ssh-deploy-after-save)
               (find-file . ssh-deploy-find-file))
        :config
        (ssh-deploy-line-mode) ;; If you want mode-line feature
        (ssh-deploy-add-menu) ;; If you want menu-bar feature
      )
```

(1) You can remove the `(add-to-list)` and `(require)` lines if you installed via `ELPA` or `MELPA` repository.

* Restart Emacs or re-evaluate your *emacs-init-script*

## Example usage

File contents `/Users/username/Web/MySite/.dir-locals.el`:

``` emacs-lisp
((nil . (
  (ssh-deploy-root-local . "/Users/username/Web/MySite/")
  (ssh-deploy-root-remote . "/ssh:myuser@myserver.com|sudo:web@myserver.com:/var/www/MySite/")
  (ssh-deploy-async . 1)
  (ssh-deploy-on-explicit-save . 1)
  (ssh-deploy-script . (lambda() (let ((default-directory ssh-deploy-root-remote))(shell-command "bash compile.sh"))))
)))
```

* Now when you save a file somewhere under the directory `/Users/username/Web/MySite/`, the script will launch and deploy the file with the remote server.
* If you press `C-c C-z x` and the current buffer is a file, you will launch a `ediff` session showing differences between local file and remote file via Tramp, or if current buffer is a directory it will open a buffer showing directory differences
w* If you press `C-c C-z f` you will **force** upload local file or directory to remote host even if they have external changes.
* If you press `C-c C-z u` you will upload local file or directory to remote host.
* If you press `C-c C-z d` you will download the current file or directory from remote host and then reload current buffer.
* If you press `C-c C-z D` you will delete the current file or directory after a confirmation on local and remote host.
* If you press `C-c C-z t` you will open a terminal with remote host in base directory via `eshell`.
* If you press `C-c C-z T` you will open a terminal with remote host in current directory via `eshell`.
* If you press `C-c C-z h` you will open a terminal with remote host in base directory via `shell`.
* If you press `C-c C-z H` you will open a terminal with remote host in current directory via `shell`.
* If you press `C-c C-z b` you will browse base directory on remote host in `dired`.
* If you press `C-c C-z B` you will browse current directory on remote host in `dired`.
* If you press `C-c C-z R` you will rename current file or directory.
* If you press `C-c C-z e` you will check for remote changes to the current file.
* If you press `C-c C-z o` you will open remote file corresponding to local file.
* If you press `C-c C-z m` you will open remote sql-mysql session on remote host.
* If you press `C-c C-z s` you will run your custom deploy script.

The local path and local root is evaluated based on their `truename` so if you use different symbolic local paths it shouldn't affect the deployment procedure.

The above configuration example uses the Emacs plug-in `use-package` which I highly recommend.

## Tramp FTP problem in macOS 10.13

macOS 10.13 removed the Darwin port of BSD `ftp` which is needed for `ange-ftp`, which is required by Tramp. You can get it back by doing this:

1. Download <https://opensource.apple.com/tarballs/lukemftp/lukemftp-16.tar.gz> or some other version from <https://opensource.apple.com/tarballs/lukemftp/>
2. Extract archive
3. Visit folder for `tnftp` inside the extracted archive in terminal
4. Type `./configure` then `make` and then `sudo make install`
5. Type `mv ./src/ftp /usr/local/bin/ftp`

## Tramp FTP doesn't read my ~/.authinfo.gpg

Ange-FTP defaults to `~/.netrc` so you need to add this to your init script:

``` elisp
(setq ange-ftp-netrc-filename "~/.authinfo.gpg")
```

## Tests

Run `make test` from plug-in folder to run tests, if you need to specify specific Emacs use export syntax i.e. `export emacs="YOUR_PATH" && make tests`

## Read more
* [Tramp](https://www.gnu.org/software/tramp/) - Transparent Remote (file) Access, Multiple Protocol
* [ELPA](https://elpa.gnu.org/) - GNU Emacs Lisp Package Archive
* [MELPA](https://melpa.org/) - Milkypostman’s Emacs Lisp Package Archive
* [Directory Variables](https://www.emacswiki.org/emacs/DirectoryVariables)
* [Ediff Mode](https://www.emacswiki.org/emacs/EdiffMode)
* [emacs-async](https://github.com/jwiegley/emacs-async)
* [use-package](https://github.com/jwiegley/use-package)
* [The Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide)
