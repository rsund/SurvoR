  # ==============================
  #
  #   clone a canvas widget
  #
  #   Source: http://wiki.tcl.tk/9168
  # ==============================

package require Tcl 8.4
package require Tk 8.4
package require canvas

package provide clonecanvas 1.0

  # ----------
  #  canvas:clone proc
  # ----------
  # parm1: canvas widget
  # parm2: clone canvas widget
  # ----------

  proc canvas:clone {canvas clone} { canvas:restore $clone [canvas:save $canvas] }

  # ----------
  #  options proc
  #
  #  return non empty options
  # ----------
  # parm: options list
  # ----------
  # return: non empty options list
  # ----------

  proc options {options} \
  {
    set res {}
    foreach option $options \
    {
      set key   [lindex $option 0]
      set value [lindex $option 4]
  # due to bugs in canvas widget, must save all options,
  # even those with empty values:
  #    if {$value != ""} { lappend res [list $key $value] }
       if {[llength $option] == 5} {lappend res [list $key $value]}
    }
    return $res
  }

  # ----------
  #  canvas:save proc
  #
  #  serialize a canvas widget
  # ----------
  # parm1: canvas widget path
  # ----------
  # return: serialized widget
  # ----------

  proc canvas:save {w} \
  {
    # canvas name
    lappend save $w
    # canvas option
    lappend save [options [$w configure]]
    # canvas focus
    lappend save [$w focus]
    # canvas items
    foreach id [$w find all] \
    {
      set item {}
      # type & id
      set type [$w type $id]
      lappend item [list $type $id]
      # coords
      lappend item [$w coords $id]
      # tags
      set tags [$w gettags $id]
      lappend item $tags
      # binds
      set binds {}
        # id binds
      set events [$w bind $id]
      foreach event $events \
      { lappend binds [list $id $event [$w bind $id $event]] }
        # tags binds
      foreach tag $tags \
      {
        set events [$w bind $tag]
        foreach event $events \
        { lappend binds [list $tag $event [$w bind $tag $event]] }
      }
      lappend item $binds
      # options
      lappend item [options [$w itemconfigure $id]]
      # type specifics
      set specifics {}
      switch $type \
      {
        arc       {}
        bitmap    {}
        image     \
        {
          # image name
          set iname [$w itemcget $id -image]
          lappend specifics $iname
          # image type
          lappend specifics [image type $iname]
          # image options
          lappend specifics [options [$iname configure]]
        }
        line      {}
        oval      {}
        polygon   {}
        rectangle {}
        text      \
        {
          foreach index {insert sel.first sel.last} \
          {
            # text indexes
            catch \
            { lappend specifics [$w index $id $index] }
          }
        }
        window    \
        {
          # window name
          set wname [$w itemcget $id -window]
          lappend specifics $wname
          # window type
          lappend specifics [string tolower [winfo class $wname]]
          # window options
          lappend specifics [options [$wname configure]]
        }
      }
      lappend item $specifics
      lappend save $item
    }
    # return serialized canvas
    return $save
  }

  # ----------
  #  canvas:restore proc
  #
  #  restore a serialized canvas widget
  # ----------
  # parm1: canvas widget path
  # parm2: serialized widget to restore
  # ----------

  proc canvas:restore {w save} \
  {
    # create canvas options
    eval canvas $w [join [lindex $save 1]]
    # items
    foreach item [lrange $save 3 end] \
    {
      foreach {typeid coords tags binds options specifics} $item \
      {
        # get type
        set type [lindex $typeid 0]
        # create bitmap or window
        switch $type \
        {
          image   \
          {
            foreach {iname itype ioptions} $specifics break
            if {![image inuse $iname]} \
            { eval image create $itype $iname [join $ioptions] }
          }
          window  \
          {
            foreach {wname wtype woptions} $specifics break
            if {![winfo exists $wname]} \
            { eval $wtype $wname [join $woptions] }
            raise $wname
          }
        }
        # create item
        set id [eval $w create $type $coords -tags "{$tags}" [join $options]]
        # item bindings
        foreach bind $binds \
        {
          foreach {id event script} $bind { $w bind $id $event $script }
        }
        # item specifics
        if {$specifics != ""} \
        {
          switch $type \
          {
            text    \
            {
              foreach {insert sel.first sel.last} $specifics break
              $w icursor $id $insert
              if {${sel.first} != ""} \
              {
                $w select from $id ${sel.first}
                $w select to   $id ${sel.last}
              }
            }
          }
        }
      }
    }
    # focused item
    set focus [lindex $save 2]
    if {$focus != ""} \
    {
      $w focus [lindex $save 2]
      focus -force $w
    }
    # return path
    return $w
  }

  # ----------
  #  canvas:dump proc
  #
  #  dump a canvas widget
  # ----------
  # parm: canvas widget path
  # ----------
  # return: widget dump
  # ----------

  proc canvas:dump {w} \
  {
    set w [canvas:save $w]
    # canvas name
    lappend res [lindex $w 0]
    # canvas options
    foreach option [lindex $w 1] { lappend res [join $option \t] }
    # focused item
    lappend res [join [lindex $w 2] \t]
    # items
    foreach item [lrange $w 3 end] \
    {
      foreach {type coords tags binds options specifics} $item \
      {
        # item type
        lappend res [join $type \t]
        # item coords
        lappend res \tcoords\t$coords
        # item tags
        lappend res \ttags\t$tags
        # item bindings
        lappend res \tbinds
        foreach bind $binds { lappend res \t\t$bind }
        # item options
        lappend res \toptions
        foreach option $options \
        {
          set key [lindex $option 0]
          set value [lindex $option 1]
          lappend res \t\t$key\t$value
        }
        # item specifics
        if {$specifics != ""} \
        {
          lappend res \tspecifics
          foreach specific $specifics \
          {
            if {[llength $specific] == 1}  { lappend res \t\t$specific } \
            else { foreach token $specific { lappend res \t\t$token } }
          }
        }
      }
    }
    # return dump
    return [join $res \n]
  }



## =========
#  #   demo
#  # =========
#
#  # create initial canvas
#  pack [frame .f]
#  set c .f.c
#  pack [canvas $c]
#  $c create arc 10 10 100 100 -extent 60 -start 30 -style pieslice -tag arc
#  $c bind arc <1> { arc_script }
#  $c create bitmap 10 30 -bitmap question
#  image create photo img1 -data \
#  {
#    R0lGODdhCQAJAIAAAASCBPz+/CwAAAAACQAJAAACEYwPp5Aa3BBcMJrqHsua
#    P1MAADs=
#  }
#  image create photo img2 -file left.gif
#  $c create image 40 125 -image img1 -tag {img _img_}
#  set id [$c create image 60 125 -image img2 -tag {img _img_}]
#  $c bind img   <ButtonPress> { img_script_button }
#  $c bind img   <KeyPress>    { img_script_key }
#  $c bind _img_ <KeyPress>    { _img_script_key }
#  $c bind $id <KeyPress> { img_script_key_id }
#  set data "#define v_width 8\n#define v_height 4"
#  append data { static unsigned char v_bits[] = { 0x18, 0x3c, 0x7e, 0xff }; }
#  image create bitmap bmp -data $data
#  $c create image 40 70 -image bmp
#  $c create line 10 10 50 50 100 10 150 50
#  $c create oval 10 10 100 100
#  $c create polygon 10 100 50 50 100 100 150 50
#  $c create rectangle 10 10 150 150
#  $c create text 120 120 -text "test" -font {Courier 16}
#  label .f.l -text Label
#  $c create window 50 50 -window .f.l
#  # clone canvas
#  set c2 .f.c2
#  pack [canvas:clone $c $c2]
#  update
#  # after waiting,
#  after 5000
#  # save old canvas
#  set save [canvas:save $c2]
#  # delete it
#  destroy $c $c2
#  # restore it
#  pack [canvas:restore $c $save]
