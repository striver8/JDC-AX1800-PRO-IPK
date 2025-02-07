(defun c:AreaLabel (/ *error* rndStr ss lst fList pList grName ent obj areaVal pt mText fieldCode minPt maxPt)
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acadObj))
  
  ;; 错误处理函数
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
      (princ (strcat "\n错误: " msg)))
    (vla-EndUndoMark doc)
    (princ))
  
  ;; 改进的随机字符串函数（替换RAND）
  (defun rndStr (/ chars result timePart)
    (setq chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
    (setq timePart (rtos (* 1000000 (getvar "TDUSRTIMER")) 2 0)) ; 基于时间的随机种子
    (repeat 6
      (setq result (cons (substr chars (1+ (rem (atoi (substr timePart (setq i (1+ (rem (strlen timePart) 6))) 1)) 62)) 1) result)))
    (apply 'strcat result))
  
  ;; 主程序开始
  (vla-StartUndoMark doc)
  
  ;; 扩展选择过滤器（支持传统POLYLINE）
  (if (setq ss (ssget '((-4 . "<OR")
                         (0 . "HATCH")
                         (-4 . "<OR")
                           (-4 . "<AND")
                             (0 . "LWPOLYLINE")
                             (-4 . "&") (70 . 1)
                           (-4 . "AND>")
                           (-4 . "<AND")
                             (0 . "POLYLINE")
                             (-4 . "&") (70 . 1)
                           (-4 . "AND>")
                         (-4 . "OR>")
                         (-4 . "OR>"))))
    (progn
      ;; 分类处理对象
      (setq lst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
      (foreach ent lst
        (cond
          ((= (cdr (assoc 0 (entget ent))) "HATCH")
           (setq fList (cons ent fList)))
          ((wcmatch (cdr (assoc 0 (entget ent))) "LWPOLYLINE,POLYLINE")
           (setq pList (cons ent pList)))))
      
      ;; 处理图案填充
      (foreach ent fList
        (setq obj (vlax-ename->vla-object ent)
              areaVal (vla-get-Area obj))
        ;; 改进的边界框计算
        (vla-GetBoundingBox obj 'minPt 'maxPt)
        (setq pt (mapcar '/ (mapcar '+ (vlax-safearray->list minPt)
                                         (vlax-safearray->list maxPt))
                             '(2 2 2)))
        ;; 创建字段文字
        (setq fieldCode (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                                (itoa (vla-get-ObjectID obj)) ">%).Area \\f \"%lu2%pr2\">%"))
        (setq mText (vla-AddMText (vla-get-ModelSpace doc) (vlax-3d-point pt) 0 fieldCode))
        (vla-put-Color mText 1)
        (vla-put-Height mText 500)
        (vla-put-Width mText (* 500 0.425))
        ;; 使用句柄替代Name属性
        (setq grName (strcat "HATCH_" (vla-get-Handle obj) "_" (rndStr)))
        (command "_.-group" "_C" grName "_A" ent (vlax-vla-object->ename mText) ""))
      
      ;; 处理闭合多段线
      (foreach ent pList
        (setq obj (vlax-ename->vla-object ent)
              areaVal (vla-get-Area obj))
        (vla-Highlight obj :vlax-true)
        (princ (strcat "\n处理对象: " (cdr (assoc 0 (entget ent))) " 面积: " (rtos areaVal 2 2)))
        (if (setq pt (getpoint "\n指定标注位置: "))
          (progn
            (setq fieldCode (strcat "%<\\AcObjProp Object(%<\\_ObjId "
                                    (itoa (vla-get-ObjectID obj)) ">%).Area \\f \"%lu2%pr2\">%"))
            (setq mText (vla-AddMText (vla-get-ModelSpace doc) (vlax-3d-point pt) 0 fieldCode))
            (vla-put-Color mText 1)
            (vla-put-Height mText 500)
            (vla-put-Width mText (* 500 0.425))
            ;; 使用面积值+句柄生成唯一名称
            (setq grName (strcat (rtos areaVal 2 0) "_" (vla-get-Handle obj) "_" (rndStr)))
            (command "_.-group" "_C" grName "_A" ent (vlax-vla-object->ename mText) "")))
        (vla-Highlight obj :vlax-false))
      ))
  
  (vla-EndUndoMark doc)
  (princ))