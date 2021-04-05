#ifndef __core__
   #define __core__
   
   #include once "platform.bi"
   
   /'
      as FB doesn't implements the class keyword, this is used to distinguish
      between a class (object) and a type (data)
   '/
   #ifdef class
      #undef class
      #define class type
   #endif
   
   '' null for objects
   #ifdef nothing
      #undef nothing
      #define nothing 0
   #else
      #define nothing 0
   #endif
   
   /'
      again, FB doesn't have the 'inherits' keyword to differentiate interface
      inheritance and implementation inheritance. This define is useful to remember
      that
      
      syntactically, they are the same. Conceptually, they are very different
   '/
   #ifdef inherits
      #undef inherits
      #define inherits extends
   #else
      #define inherits extends
   #endif

   /'
      FB also don't distinguish between an interface class and a class implementation
      this keyword is used for that sole purpose
   '/
   #ifdef interface
      #undef interface
      #define interface type
   #else
      #define interface type
   #endif
#endif

