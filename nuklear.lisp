(cl:in-package :nuklear)

;;----------------------------------------------------------------------

(defvar *max-vertex-buffer-size* (* 512 1024))
(defvar *max-element-buffer-size* (* 128 1024))

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
   (max-element-buffer :initarg :max-element-buffer)))

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
    (setf atlas )
    (%nk:font-atlas-init-default atlas)
    (%nk:font-atlas-begin atlas)
    renderer))

(defun bodge-font-stash-end (renderer nk-context)
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
                      max-vertex-buffer max-element-buffer)
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
        (bodge-font-stash-end rndr)
        rndr))))

(defun bodge-renderer-font (renderer)
  (claw:c-let () ))

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
  (%nk:bodge-renderer-destroy (nk-renderer-handle renderer)))


(defun render-nuklear (renderer context width height &optional (pixel-ratio 1f0))
  (%nk:bodge-render context renderer
                    (floor width) (floor height) (float pixel-ratio 0f0)))
