function thing(x) {
  return $('<div/>').addClass(x)
}


function card(w,c) {
  var dom = thing('card')
          .css('width',w)
          .css('height',1.5 * w)
          .css('font-size', w / 10)
          .addClass(c.color)
          .addClass(c.rarity)

  var t = thing('card-title')
        .text(c.title)
  var i = thing('card-image')
        .css('background-image','url("' + c.image + '")')

  var d = thing('card-text')

  var co = thing('card-cost')
        .text(c.cost)

  jQuery.each(c.description,function(i,l) {
    d.append($('<span/>').text(l))
  })
  return dom.append(t,i,co,d)
}

function smallCard(c) {
  var dom = card(128,c)
  dom.click(function() {
    dom.toggleClass('selected')
  })
  return dom
}


