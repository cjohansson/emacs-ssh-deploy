# `ssh-deploy`

The `ssh-deploy` plugin makes it possible to effortlessly deploy local files to remote hosts via SSH. It also makes it possible to define remote paths per directory and whether or not you want to deploy on explicit save actions or not. Also it enables manual upload and download of files. You can also check differences between local file and remote file if you have `tramp` installed. **You need to have a setup which allows password-less key-based logins to servers via SSH and have scp installed locally**.

`ssh-deploy` works with `DirectoryVariables` so you can have different deploy setups in different ways for different folders.

The idea for this plug-in was to mimic the behavior of PhpStorm deployment functionality.

This application is made by Christian Johansson <christian@cvj.se> 2016 and is licensed under GNU General Public License 3.


## A setup example

* Download ssh-deploy and place it at `~/.emacs.d/ssh-deploy/`.

* Create this `DirectoryVariables` file in your project root at `/Users/username/Web/MySite/.dir-locals.el`.

``` elisp
((nil . ((ssh-deploy-root-local . "/Users/username/Web/MySite/")
(ssh-deploy-root-remote . "/Volumes/myserver.com/MySite/")
(ssh-deploy-on-explicity-save . t))))
```

* And add this to your *emacs-init-script*:

``` elisp
;; ssh-deploy - prefix = C-c C-z, u = upload, d = download, x = diff
(add-to-list 'load-path "~/.emacs.d/ssh-deploy/")
(use-package ssh-deploy
  :config
  (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicity-save (ssh-deploy-upload-handler)) ))
  (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
  (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler)(revert-buffer) ))
  (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) )))
```

* Now when you save a file somewhere under the root `/Users/username/Web/MySite/`, the script will launch and deploy the file with the remote server.
* If you press `C-c C-z x` you will launch a `ediff` session showing differences between local file and remote file using `tramp`.
* If you press `C-c C-z u` you will upload local file to remote host.
* If you press `C-c C-z d` you will download file from remote host and reload current buffer.

The above configuration uses the plugin `use-package` which I highly recommend.

## TODO

* Add a message if remote contents has changed since last upload
* Add support for specific SSH identity files
* Add support for specific SSH ports
* Add support for directories

## Read more
* <https://www.emacswiki.org/emacs/DirectoryVariables>
* <http://www.gnu.org/software/tramp/>
* <https://github.com/jwiegley/use-package>
* <https://www.emacswiki.org/emacs/EdiffMode>
