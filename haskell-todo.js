$(document).ready(function($) {
    //set current id from hash
    if(window.location.hash) {
       HaskellTodo.setCurrentListId(window.location.hash.substring(1));
       HaskellTodo.items.get();
    }

    $('#create-new-list').on('click', function() {
        HaskellTodo.list.create();
        return false;
    });

    $('#destroy-list').on('click', function() {
        HaskellTodo.list.destroy();
        return false;
    });

    $('form#todo').submit(function() {
        if ($('input').val() !== '') {
            var item_text = $('input').val();

            HaskellTodo.items.add(item_text);
        };
        $('input').val('');
        return false;
    });

    $(document).on('click', 'a.delete-todo', function (e) {
        HaskellTodo.items.delete($(this).parent().data('itemId'));
        e.preventDefault();
        $(this).parent().remove();
    });

    makeEditable();
});

function makeEditable() {
  $('.editable').editable(function(value, settings) { 
     HaskellTodo.items.edit($(this).parent().data('itemId'), value);
     return(value);
  });
}                 

var HaskellTodo = {
    items : {
      _list_id : null, 
      _list_version : 0,

      setListId : function(list_id) {
        this._list_id = list_id;
      },

      getListId : function() {
        return this._list_id;
      },

      setListVersion : function(list_version) {
        this._list_version = list_version;
      },

      add : function(item_text) {
        $.ajax({
          url : this._prefixListUrl('items'),
          dataType : "json",
          type : "POST",
          data : JSON.stringify({'text' : item_text})
        }).done(
          function(e) {
            //add item to DOM                 
            HaskellTodo.items.addToDom(e);
          }
        );
      },

      edit : function(item_id, text) {
        $.ajax({
          url : this._prefixListUrl('items/' + item_id),
          dataType : "json",
          type : "PATCH",
          data : JSON.stringify({'text' : text, 'done' : false})
        }).done(
          function(e) {
            
          }
        );
      },

      delete : function(item_id) {
        $.ajax({
          url : this._prefixListUrl('items/' + item_id),
          dataType : "json",
          type : "DELETE",
          data : ''
        });                
      },

      addToDom : function(item) {
        $('ul#todo-list').append('<li data-item-id="' + item.item_id + '"><span class="editable">' + item.text + '</span> <a class="delete-todo" href="javascript: void();">x</a></li>');
        makeEditable();  
      },

      addAll : function(items) {
        $('ul#todo-list').empty();
        for(var i=0; i < items.length; i++) {
          this.addToDom(items[i]);
        }
      },

      get : function() {        
        if(!this._list_id) { return; }
        $.ajax({
           url : this._prefixListUrl(),
           dataType: "json",
           data: "when_none_match=" + this._list_version,
           success: function(e) {
             HaskellTodo.items.setListVersion(e.version);
             HaskellTodo.items.addAll(e.items);
           },
           complete: function() { HaskellTodo.items.get(); },
           timeout: 30000
         });
         return false;
      },

      _prefixListUrl : function(path) {
        var url = '/lists/' + this._list_id;
        if(path) {
          url += '/' + path;
        }
        return url;
      }  
    },
    
    getHostUrl : function() {
      return 'http://' + window.location.hostname + ':' + window.location.port;
    },

    setCurrentListId : function(list_id) {
      this.items.setListId(list_id);
      var url = HaskellTodo.getHostUrl() + '/#' + list_id;    
      $('#invite-url span').html('<a href="' + url  + '">' + url + '</a>');
      $('#invite-url').show();
      $('#create-todo').show();
    },

    list : {
      create : function() {

        $.ajax({
          url : '/lists',
          type : 'POST',
          dataType : "json",
          data : ''
        }).done(
          function(e) {
            HaskellTodo.setCurrentListId(e.list_id);
            HaskellTodo.items.get();
          }
        );
      },

      destroy : function() {

        $.ajax({
          url : '/lists/' + HaskellTodo.items.getListId(),
          type : 'DELETE',
          dataType : "json",
          data : ''
        }).done(
          function(e) {
            location.href = HaskellTodo.getHostUrl();
          }
        );
      },

    }        
};

HaskellTodo.items.get();
