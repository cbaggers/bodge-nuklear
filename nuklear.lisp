(cl:in-package :nuklear)

;;----------------------------------------------------------------------

(defvar *max-vertex-buffer-size* (* 512 1024))
(defvar *max-element-buffer-size* (* 128 1024))

;;----------------------------------------------------------------------

(cffi:defcfun (%memset "memset") :pointer
  (destination-pointer :pointer)
  (val :int)
  (byte-length :long))

(cffi:defcfun (%memcpy "memcpy") :pointer
  (destination-pointer :pointer)
  (source-pointer :pointer)
  (byte-length :long))

;;----------------------------------------------------------------------

(cffi:defcstruct nk-bodge-vertex
  (position (:array :float 2))
  (texture (:array :float 2))
  (color (:array :uint8 4)))

(defclass nuklear-renderer ()
  ((cmds :initarg :cmds
         :initform (claw:calloc '(:struct (%nk:buffer))))
   (null-tex :initarg :null-tex
             :initform (claw:calloc '(:struct (%nk:draw-null-texture))))
   (atlas :initarg :atlas
          :initform (claw:calloc '(:struct (%nk:font-atlas))))
   (vbo :initarg :vbo)
   (vao :initarg :vao)
   (ebo :initarg :ebo)
   (prog :initarg :prog)
   (vert-shdr :initarg :vert-shdr)
   (frag-shdr :initarg :frag-shdr)
   (attrib-pos :initarg :attrib-pos)
   (attrib-uv :initarg :attrib-uv)
   (attrib-col :initarg :attrib-col)
   (uniform-tex :initarg :uniform-tex)
   (uniform-proj :initarg :uniform-prpoj)
   (font-tex :initarg :font-tex)
   (max-vertex-buffer :initarg :max-vertex-buffer)
   (max-element-buffer :initarg :max-element-buffer)
   ;;
   (vertex-layout-ptr
    :initarg :vertex-layout-ptr
    :initform (claw:calloc '(:struct (%nk:draw-vertex-layout-element)) 4))))

(defun bodge-renderer-upload-atlas (renderer image-ptr width height)
  (with-slots (font-tex) renderer
    (setf font-tex (gl:gen-texture))
    (gl:bind-texture :texture-2d font-tex)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba
                     :unsigned-byte image-ptr)
    renderer))

(defun bodge-font-stash-begin (renderer)
  (with-slots (atlas) renderer
    (%nk:font-atlas-init-default atlas)
    (%nk:font-atlas-begin atlas)
    renderer))

(defun bodge-font-stash-end (renderer)
  (with-slots (atlas null-tex font-tex) renderer
    (claw:c-with ((w :int)
                  (h :int))
      (let ((image (%nk:font-atlas-bake atlas
                                        (w :&)
                                        (h :&)
                                        %nk:+font-atlas-rgba32+)))
        (bodge-renderer-upload-atlas renderer image w h)
        (%nk:font-atlas-end atlas (%nk:handle-id font-tex) null-tex)))))

(defun bodge-renderer-create (max-vertex-buff-len max-element-buff-len)
  (let ((rndr (make-instance 'nuklear-renderer)))
    (with-slots (cmds null-tex atlas vbo vao ebo prog vert-shdr frag-shdr
                      attrib-pos attrib-uv attrib-col
                      uniform-tex uniform-proj
                      max-vertex-buffer max-element-buffer
                      vertex-layout-ptr)
        rndr
      (let* ((nk-shader-version
              #+darwin "#version 150\n"
              #-darwin "#version 300 es\n")
             (vertex-shader
              (concatenate
               'string
               nk-shader-version
               "uniform mat4 ProjMtx;\n"
               "in vec2 Position;\n"
               "in vec2 TexCoord;\n"
               "in vec4 Color;\n"
               "out vec2 Frag_UV;\n"
               "out vec4 Frag_Color;\n"
               "void main() {\n"
               "   Frag_UV = TexCoord;\n"
               "   Frag_Color = Color;\n"
               "   gl_Position = ProjMtx * vec4(Position.xy, 0, 1);\n"
               "}\n"))
             (fragment-shader
              (concatenate
               'string
               nk-shader-version
               "precision mediump float;\n"
               "uniform sampler2D Texture;\n"
               "in vec2 Frag_UV;\n"
               "in vec4 Frag_Color;\n"
               "out vec4 Out_Color;\n"
               "void main(){\n"
               "   Out_Color = Frag_Color * texture(Texture, Frag_UV.st);\n"
               "}\n")))
        (setf max-vertex-buffer max-vertex-buff-len
              max-element-buffer max-element-buff-len)
        (%nk:buffer-init-default cmds)
        (setf prog (gl:create-program)
              vert-shdr (gl:create-shader :vertex-shader)
              frag-shdr (gl:create-shader :fragment-shader))
        (gl:shader-source vert-shdr vertex-shader)
        (gl:shader-source frag-shdr fragment-shader)
        (gl:compile-shader vert-shdr)
        (gl:compile-shader frag-shdr)
        (assert (gl:get-shader vert-shdr :compile-status))
        (assert (gl:get-shader frag-shdr :compile-status))
        (gl:attach-shader prog vert-shdr)
        (gl:attach-shader prog frag-shdr)
        (gl:link-program prog)
        (assert (gl:get-program prog :link-status))

        (setf uniform-tex (gl:get-uniform-location prog "Texture")
              uniform-proj (gl:get-uniform-location prog "ProjMtx")
              attrib-pos (gl:get-uniform-location prog "Position")
              attrib-uv (gl:get-uniform-location prog "TexCoord")
              attrib-col (gl:get-uniform-location prog "Color"))

        (let* ((vs (cffi:foreign-type-size '(:struct nk-bodge-vertex)))
               (vp (cffi:foreign-slot-offset '(:struct nk-bodge-vertex)
                                             'position))
               (vt (cffi:foreign-slot-offset '(:struct nk-bodge-vertex)
                                             'uv))
               (vc (cffi:foreign-slot-offset '(:struct nk-bodge-vertex)
                                             'col)))
          (setf vbo (gl:gen-buffer))
          (setf ebo (gl:gen-buffer))
          (setf vao (gl:gen-vertex-array))

          (gl:bind-vertex-array vao)
          (gl:bind-buffer :array-buffer vbo)
          (gl:bind-buffer :element-array-buffer ebo)

          (gl:enable-vertex-attrib-array attrib-pos)
          (gl:enable-vertex-attrib-array attrib-uv)
          (gl:enable-vertex-attrib-array attrib-col)

          (gl:vertex-attrib-pointer attrib-pos 2 :float nil vs vp)
          (gl:vertex-attrib-pointer attrib-uv 2 :float nil vs vt)
          (gl:vertex-attrib-pointer attrib-col 4 :uint8 t vs vc))

        (gl:bind-texture :texture-2d 0)
        (gl:bind-buffer :array-buffer 0)
        (gl:bind-buffer :element-array-buffer 0)
        (gl:bind-vertex-array 0)

        (bodge-font-stash-begin rndr)
        (bodge-font-stash-end rndr))

      (c-let ((lptr (:struct (%nk:draw-vertex-layout-element))
                    :from vertex-layout-ptr))
        (setf (lptr 0 :attribute) %nk:+vertex-position+)
        (setf (lptr 0 :format) %nk:+format-float+)
        (setf (lptr 0 :offset) (cffi:foreign-slot-offset
                                '(:struct nk-bodge-vertex)
                                'position))
        (setf (lptr 1 :attribute) %nk:+vertex-texcoord+)
        (setf (lptr 1 :format) %nk:+format-float+)
        (setf (lptr 1 :offset) (cffi:foreign-slot-offset
                                '(:struct nk-bodge-vertex)
                                'uv))

        (setf (lptr 2 :attribute) %nk:+vertex-color+)
        (setf (lptr 2 :format) %nk:+format-r8g8b8a8+)
        (setf (lptr 2 :offset) (cffi:foreign-slot-offset
                                '(:struct nk-bodge-vertex)
                                'col))
        (setf (lptr 3 :attribute) %nk:+vertex-layout-end+)))
    rndr))

(defun bodge-renderer-font (renderer)
  (with-slots (atlas) renderer
    (claw:c-let ((nk-atlas (:struct (%nk:font-atlas)) :from atlas)
                 (font (:struct (%nk:font)) :from (nk-atlas :default-font)))
      ;;(font :handle :&) dont think so as cbv will return it as a ptr
      (font :handle))))

;; (claw:c-let ((nk-atlas (:struct (%nk:font-atlas)) :from atlas)
;;                    (font (:struct (%nk:font)) :from (nk-atlas :default-font)))
;;         (unless (claw:null-pointer-p (nk-atlas :default-font))
;;           (%nk:style-set-font nk-context (font :handle :&))))

(defun make-renderer ()
  (bodge-renderer-create *max-vertex-buffer-size* *max-element-buffer-size*))

(defun renderer-font (renderer)
  (bodge-renderer-font renderer))

;;----------------------------------------------------------------------



(define-bitmask-from-enum (panel-flags (:enum (%nk:panel-flags))))


(defmacro with-vec2 ((vec) &body body)
  `(claw:c-with ((,vec (:struct (%nk:vec2)) :calloc t))
     ,@body))


(defun panel-mask (&rest opts)
  (apply #'mask 'panel-flags opts))


(defmacro define-text-width-callback (name (handle font-height text) &body body)
  (with-gensyms (f-text len)
    `(defcallback ,name :float ((,handle :pointer)
                                (,font-height :float)
                                (,f-text :pointer)
                                (,len :int))
       (declare (ignorable ,handle ,font-height))
       (let ((,text (cffi:foreign-string-to-lisp ,f-text :count ,len)))
         (float (progn ,@body) 0f0)))))


(defun make-user-font (height width-callback &optional (user-data-ptr (cffi:null-pointer)))
  (c-let ((fnt (:struct (%nk:user-font)) :calloc t))
    (setf (fnt :userdata :ptr) user-data-ptr
          (fnt :width) (callback width-callback)
          (fnt :height) (float height 0f0))
    fnt))


(defun destroy-user-font (font)
  (free font))


(defun make-context (&optional font)
  (c-let ((ctx (:struct (%nk:context)) :calloc t))
    (%nk:init-default ctx font)
    ctx))


(defun destroy-context (ctx)
  (unwind-protect
       (%nk:free ctx)
    (free ctx)))


(defun command-type (cmd)
  (claw:enum-key '(:enum (%nk:command-type)) (c-ref cmd (:struct (%nk:command)) :type)))


(defmacro docommands ((cmd ctx) &body body)
  (once-only (ctx)
    `(loop for ,cmd = (%nk:command-list-begin ,ctx) then (%nk:command-list-next ,ctx ,cmd)
           until (cffi-sys:null-pointer-p (claw:ptr ,cmd))
           do (progn ,@body)
           finally (return (values)))))

;;;
;;; RENDERING
;;;


(defun destroy-renderer (renderer)
  (with-slots (vert-shdr frag-shdr prog font-tex vbo ebo cmds atlas) renderer
    (gl:detach-shader prog vert-shdr)
    (gl:detach-shader prog frag-shdr)
    (gl:delete-shader vert-shdr)
    (gl:delete-shader frag-shdr)
    (gl:delete-program prog)
    (gl:delete-texture font-tex)
    (gl:delete-buffers (list vbo ebo))
    (%nk:buffer-free cmds)
    (%nk:buffer-free atlas))
  nil)

(declaim
 (type (simple-array single-float (16)) *ortho*))
(defvar *ortho*
  (make-array 16 :element-type :float
              :initial-contents
              '(2.0f0 0.0f0 0.0f0 0.0f0 
                0.0f0 -2.0f0 0.0f0 0.0f0 
                0.0f0 0.0f0 -1.0f0 0.0f0 
                -1.0f0 1.0f0 0.0f0 1.0f0)))

(defun render-nuklear (renderer context width height &optional (pixel-ratio 1f0))
  (with-slots (prog uniform-tex uniform-proj vao vbo ebo
                    max-vertex-buffer max-element-buffer
                    null-tex cmds vertex-layout-ptr)
      renderer
    (let* ((width (floor width))
           (height (floor height))
           (pixel-ratio (float pixel-ratio 0f0))

           (aa %nk:+anti-aliasing-on+)
           (ortho *ortho*))
      (setf (aref *ortho* 0) (float width 0f0)
            (aref *ortho* 5) (float height 0f0))

      ;; setup global state
      (gl:enable :blend)
      (gl:blend-equation :func-add)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:disable :cull-face)
      (gl:disable :depth-test)
      (gl:enable :scissor-test)
      (gl:active-texture :texture0)

      ;; setup program
      (gl:use-program prog)
      (%gl:uniform-1i uniform-tex 0)
      (cffi:with-pointer-to-vector-data (ortho-ptr ortho)
        (%gl:uniform-matrix-4fv uniform-proj 1 nil ortho-ptr))
      (gl:viewport 0 0 (floor (* width pixel-ratio))
                   (floor (* height pixel-ratio)))

      ;; convert from command queue into draw list and draw to screen
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      (gl:bind-buffer :element-array-buffer ebo)

      (%gl:buffer-data :array-buffer
                       max-vertex-buffer (cffi:null-pointer) :stream-draw)
      (%gl:buffer-data :element-array-buffer
                       max-element-buffer (cffi:null-pointer) :stream-draw)

      ;; load draw vertices & elements directly into vertex + element buffer
      (let ((vertices (gl:map-buffer :array-buffer :write-only))
            (elements (gl:map-buffer :element-array-buffer :write-only)))
        (c-with ((config (:struct (%nk:convert-config))))
          ;; fill convert configuration
          (%memset (config :&) 0 #.(claw:sizeof '(:struct (%nk:convert-config))))
          (setf (config :vertex-layout) vertex-layout-ptr)
          (setf (config :vertex-size)
                #.(cffi:foreign-type-size '(:struct nk-bodge-vertex)))
          (setf (config :vertex-alignment)
                #.(cffi:foreign-type-alignment '(:struct nk-bodge-vertex)))
          (%memcpy (config :null :&) null-tex
                   #.(claw:sizeof '(:struct (%nk:draw-null-texture))))
          (setf (config :circle-segment-count) 22)
          (setf (config :curve-segment-count) 22)
          (setf (config :arc-segment-count) 22)
          (setf (config :global-alpha) 1.0f0)
          (setf (config :shape-aa) aa)
          (setf (config :line-aa) aa)

          ;; setup buffers to load vertices and elements
          (claw:c-with ((vbuf (:struct (%nk:buffer)))
                        (ebuf (:struct (%nk:buffer))))
            (%nk:buffer-init-fixed (vbuf :&) vertices max-vertex-buffer)
            (%nk:buffer-init-fixed (ebuf :&) elements max-element-buffer)
            (%nk:convert context cmds (vbuf :&) (ebuf :&) (config :&)))))

      (gl:unmap-buffer :array-buffer)
      (gl:unmap-buffer :element-array-buffer)

      ;; iterate over and execute each draw command

      (loop
         :with offset := (cffi:make-pointer 0)
         :for cmd-ptr := (%nk:draw-list-begin context cmds)
         :while (not (cffi:null-pointer-p cmd-ptr))
         :do
           (c-let ((cmd (:struct (%nk:draw-command)) :from cmd-ptr))
             (let ((elem-count (cmd :elem-count)))
               (when (> (cmd :elem-count) 0)
                 (gl:bind-texture :texture-2d (cmd :texture :id))
                 (gl:scissor (floor (* (cmd :clip-rect :x) pixel-ratio))
                             (floor (* (- height (+ (cmd :clip-rect :y)
                                                    (cmd :clip-rect :h)))
                                       pixel-ratio))
                             (floor (* (cmd :clip-rect :w) pixel-ratio))
                             (floor (* (cmd :clip-rect :h) pixel-ratio)))
                 (%gl:draw-elements
                  :triangles elem-count :unsigned-short offset)
                 (cffi:incf-pointer offset elem-count))))
           (%nk:draw-list-next cmd-ptr cmds context))

      ;; default OpenGL state
      (gl:use-program 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :element-array-buffer 0)
      (gl:bind-vertex-array 0)
      (gl:disable :blend)
      (gl:disable :scissor-test)
      t)))
