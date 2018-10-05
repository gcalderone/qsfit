;---------------------------------------------------------------------
FUNCTION greformstruct $
   , s                          ;structure of arrays to be reformed.
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error
 
  ;;structure of arrays --> array of structures
  tagn = TAG_NAMES(s)
  nf = N_TAGS(s)

  ;;Check each arrays have same number of elements
  n = []
  FOR i=0, nf-1 DO $
     n = [n, gn(s.(i))]

  ;;If fields have different size return original struct
  IF (MIN(n) NE (MAX(n))) THEN RETURN, s 

   
  ;;Prepare base structure
  ret = []
  FOR i=0, nf-1 DO $
     ret = gstru_insert(ret, tagn[i], (s.(i))[0])
  
  ;;Array of structures
  ret = REPLICATE(ret, n[0])
  
  ;;Copy data
  FOR i=0, nf-1 DO $
     ret.(i) = s.(i)

  RETURN, ret
END
