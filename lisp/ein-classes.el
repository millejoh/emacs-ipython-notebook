;;; ein-classes.el --- Classes and structures.

;; Copyright (C) 2017 John M. Miller

;; Author: John M Miller <millejoh at mac dot com>

;; This file is NOT part of GNU Emacs.

;; ein-classes.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-classes.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-worksheet.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Content
(require 'eieio)

(defstruct ein:$content
  "Content returned from the Jupyter notebook server:
`ein:$content-url-or-port'
  URL or port of Jupyter server.

`ein:$content-name'
  The name/filename of the content. Always equivalent to the last
  part of the path field

`ein:$content-path'
 The full file path. It will not start with /, and it will be /-delimited.

`ein:$content-type'
 One of three values: :directory, :file, :notebook.

`ein:$content-writable'
  Indicates if requester has permission to modified the requested content.

`ein:$content-created'

`ein:$content-last-modified'

`ein:$content-mimetype'
  Specify the mime-type of :file content, null otherwise.

`ein:$content-raw-content'
  Contents of resource as returned by Jupyter.  Depending on content-type will hold:
    :directory : JSON list of models for each item in the directory.
    :file      : Text of file as a string or base64 encoded string if mimetype
                 is other than 'text/plain'.
    :notebook  : JSON structure of the file.

`ein:$content-format'
  Value will depend on content-type:
    :directory : :json.
    :file      : Either :text or :base64
    :notebook  : :json.

`ein:$content-checkpoints'
  Names auto-saved checkpoints for content. Stored as a list
  of (<id> . <last_modified>) pairs.
"
  url-or-port
  notebook-version
  name
  path
  type
  writable
  created
  last-modified
  mimetype
  raw-content
  format
  session-p
  checkpoints)



;;; Websockets

(defstruct ein:$websocket
  "A wrapper object of `websocket'.

`ein:$websocket-ws'               : an instance returned by `websocket-open'
`ein:$websocket-kernel'           : kernel at the time of instantiation
`ein:$websocket-closed-by-client' : t/nil'
"
  ws
  kernel
  closed-by-client)


;;; Notebook
(defstruct ein:$notebook
  "Hold notebook variables.

`ein:$notebook-url-or-port'
  URL or port of IPython server.

`ein:$notebook-notebook-id' : string
  uuid string (as of ipython 2.0 this is the same is notebook-name).

`ein:$notebook-notebook-path' : string
  Path to notebook.

`ein:$notebook-kernel' : `ein:$kernel'
  `ein:$kernel' instance.

`ein:$notebook-kernelspec' : `ein:$kernelspec'
  Jupyter kernel specification for the notebook.

`ein:$notebook-kernelinfo' : `ein:kernelinfo'
  `ein:kernelinfo' instance.

`ein:$notebook-pager'
  Variable for `ein:pager-*' functions. See ein-pager.el.

`ein:$notebook-dirty' : boolean
  Set to `t' if notebook has unsaved changes.  Otherwise `nil'.

`ein:$notebook-metadata' : plist
  Notebook meta data (e.g., notebook name).

`ein:$notebook-name' : string
  Notebook name.

`ein:$notebook-nbformat' : integer
  Notebook file format version.

`ein:$notebook-nbformat-minor' : integer
  Notebook file format version.

`ein:$notebook-events' : `ein:$events'
  Event handler instance.

`ein:$notebook-worksheets' : list of `ein:worksheet'
  List of worksheets.

`ein:$notebook-scratchsheets' : list of `ein:worksheet'
  List of scratch worksheets.

`ein:$notebook-api-version' : integer
   Major version of the IPython notebook server we are talking to.

`ein:$notebook-checkpoints'
  Names auto-saved checkpoints for content. Stored as a list
  of (<id> . <last_modified>) pairs.
"
  url-or-port
  notebook-id ;; In IPython-2.0 this is "[:path]/[:name].ipynb"
  notebook-path
  kernel
  kernelinfo
  kernelspec
  pager
  dirty
  metadata
  notebook-name
  nbformat
  nbformat-minor
  events
  worksheets
  scratchsheets
  api-version
  autosave-timer
  checkpoints)



;;; Worksheet
(defclass ein:worksheet ()
  ((nbformat :initarg :nbformat :type integer)
   (get-notebook-name :initarg :get-notebook-name :type cons
                      :accessor ein:worksheet--notebook-name)
   ;; This slot introduces too much complexity so therefore must be
   ;; removed later.  This is here only for backward compatible
   ;; reason.
   (discard-output-p :initarg :discard-output-p :accessor ein:worksheet--discard-output-p)
   (saved-cells :initarg :saved-cells :initform nil
                :accessor ein:worksheet--saved-cells
                :documentation
                "Slot to cache cells for worksheet without buffer")
   (dont-save-cells :initarg :dont-save-cells :initform nil :type boolean
                    :accessor ein:worksheet--dont-save-cells-p
                    :documentation "Don't cache cells when this flag is on.")
   (ewoc :initarg :ewoc :type ewoc :accessor ein:worksheet--ewoc)
   (kernel :initarg :kernel :type ein:$kernel :accessor ein:worksheet--kernel)
   (dirty :initarg :dirty :type boolean :initform nil :accessor ein:worksheet--dirty-p)
   (metadata :initarg :metadata :initform nil :accessor ein:worksheet--metadata)
   (show-slide-data-p :initarg :show-slide-data-p
                      :initform nil
                      :accessor ein:worksheet--show-slide-data-p)
   (events :initarg :events :accessor ein:worksheet--events)))


;;; Kernel
(defstruct ein:$kernelspec
  "Kernel specification as return by the Jupyter notebook server.

`ein:$kernelspec-name' : string
  Name used to identify the kernel (like python2, or python3).

`ein:$kernelspec-display-name' : string
  Name used to display kernel to user.

`ein:$kernelspec-language' : string
  Programming language supported by kernel, like 'python'.

`ein:$kernelspec-resources' : plist
  Resources, if any, used by the kernel.

`ein:$kernelspec-spec' : plist
  How to start the kernel from the command line. Not used by ein (yet).
"
  name
  display-name
  resources
  spec
  language)

;; FIXME: Rewrite `ein:$kernel' using `defclass'.  It should ease
;;        testing since I can mock I/O using method overriding.
(defstruct ein:$kernel
  "Should perhaps be named ein:$session.  We glom session and kernel as defined by the server as just ein:$kernel in the client.

"
  url-or-port
  path
  kernelspec
  events
  api-version
  session-id
  kernel-id
  shell-channel
  iopub-channel
  websocket                             ; For IPython 3.x+
  base-url                              ; /api/kernels/
  kernel-url                            ; /api/kernels/<KERNEL-ID>
  ws-url                                ; ws://<URL>[:<PORT>]
  stdin-activep
  username
  msg-callbacks
  oinfo-cache
  ;; FIXME: Use event instead of hook.
  after-start-hook
  after-execute-hook)



;;; Cells

(defclass ein:basecell ()
  ((cell-type :initarg :cell-type :type string :accessor ein:cell-type)
   (read-only :initarg :read-only :initform nil :type boolean)
   (ewoc :initarg :ewoc :type ewoc :accessor ein:basecell--ewoc)
   (element :initarg :element :initform nil :type list
            :documentation "ewoc nodes")
   (element-names :initarg :element-names)
   (input :initarg :input :type string
          :documentation "Place to hold data until it is rendered via `ewoc'.")
   (outputs :initarg :outputs :initform nil :type list)
   (metadata :initarg :metadata :initform nil :type list :accessor ein:cell-metadata) ;; For nbformat >= 4
   (events :initarg :events :type ein:events)
   (slidetype :initarg :slidetype :initform "-" :type string)
   (cell-id :initarg :cell-id :initform (ein:utils-uuid) :type string
            :accessor ein:cell-id))
  "Notebook cell base class")

(defclass ein:codecell (ein:basecell)
  ((traceback :initform nil :initarg :traceback :type list)
   (cell-type :initarg :cell-type :initform "code")
   (kernel :initarg :kernel :type ein:$kernel :accessor ein:cell-kernel)
   (element-names :initform (:prompt :input :output :footer :slidetype))
   (input-prompt-number :initarg :input-prompt-number
                        :documentation "\
Integer or \"*\" (running state).
Implementation note:
Typed `:input-prompt-number' becomes a problem when reading a
notebook that saved "*".  So don't add `:type'!")
   (collapsed :initarg :collapsed :initform nil :type boolean)
   (running :initarg :running :initform nil :type boolean)
   (dynamic :initarg :dynamic :initform nil :type boolean
            :documentation "\
Whether cell output is evaluated dynamically or not.

Only Emacs lisp type output data will be affected by this
slot (Javascript will not be evaluated).  This value must be set
to `t' when executing cell.  See `ein:notebook-execute-cell'.
In the implantation of IPython web client it is passed around via
argument, but since it is difficult to pass argument to EWOC
pretty printer, `ein:codecell' instance holds this setting in a
slot.")
   (autoexec :initarg :autoexec :initform nil :type boolean
             :documentation "Auto-execution flag.

This cell is executed when the connected buffer is saved,
provided that (1) this flag is `t' and (2) corresponding
auto-execution mode flag in the connected buffer is `t'.")))

;; Use this cell to execute hy code in notebook running a Python kernel.
(defclass ein:hy-codecell (ein:codecell)
  ((cell-type :initarg :cell-type :initform "hy-code"))
  "Codecell that supports executing hy code from within a Python kernel.")

(defclass ein:textcell (ein:basecell)
  ((cell-type :initarg :cell-type :initform "text")
   (element-names :initform (:prompt :input :footer :slidetype))))

(defclass ein:htmlcell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "html")))

(defclass ein:markdowncell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "markdown")))

(defclass ein:rawcell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "raw")))

(defclass ein:headingcell (ein:textcell)
  ((cell-type :initarg :cell-type :initform "heading")
   (level :initarg :level :initform 1)))


;;; Notifications

(defclass ein:notification-status ()
  ((status :initarg :status :initform nil)
   (message :initarg :message :initform nil)
   (s2m :initarg :s2m))
  "Hold status and its string representation (message).")

(defclass ein:notification-tab ()
  ((get-list :initarg :get-list :type function)
   (get-current :initarg :get-current :type function)
   (get-name :initarg :get-name :type function)
   (get-buffer :initarg :get-buffer :type function)
   (delete :initarg :delete :type function)
   (insert-prev :initarg :insert-prev :type function)
   (insert-next :initarg :insert-next :type function)
   (move-prev :initarg :move-prev :type function)
   (move-next :initarg :move-next :type function)
   )
  ;; These "methods" are for not depending on what the TABs for.
  ;; Probably I'd want change this to be a separated Emacs lisp
  ;; library at some point.
  "See `ein:notification-setup' for explanation.")

(defclass ein:notification ()
  ((buffer :initarg :buffer :type buffer :document "Notebook buffer")
   (tab :initarg :tab :type ein:notification-tab)
   (execution-count
    :initform "y" :initarg :execution-count
    :documentation "Last `execution_count' sent by `execute_reply'.")
   (notebook
    :initarg :notebook
    :initform
    (ein:notification-status
     "NotebookStatus"
     :s2m
     '((notebook_saving.Notebook       . "Saving Notebook...")
       (notebook_create_checkpoint.Notebook . "Creating Checkpoint...")
       (notebook_saved.Notebook        . "Notebook is saved")
       (notebook_checkpoint_created.Notebook . "Checkpoint created.")
       (notebook_save_failed.Notebook  . "Failed to save Notebook!")))
    :type ein:notification-status)
   (kernel
    :initarg :kernel
    :initform
    (ein:notification-status
     "KernelStatus"
     :s2m
     '((status_idle.Kernel . nil)
       (status_busy.Kernel . "Kernel is busy...")
       (status_restarting.Kernel . "Kernel restarting...")
       (status_restarted.Kernel . "Kernel restarted")
       (status_dead.Kernel . "Kernel requires restart \\<ein:notebook-mode-map>\\[ein:notebook-restart-session-command]")
       (status_reconnecting.Kernel . "Kernel reconnecting...")
       (status_reconnected.Kernel . "Kernel reconnected")
       (status_disconnected.Kernel . "Kernel requires reconnect \\<ein:notebook-mode-map>\\[ein:notebook-reconnect-session-command]")))
    :type ein:notification-status))
  "Notification widget for Notebook.")


;;; Events

(defclass ein:events ()
  ((callbacks :initarg :callbacks :type hash-table
              :initform (make-hash-table :test 'eq)))
  "Event handler class.")


(provide 'ein-classes)

;;; ein-classes.el ends here
