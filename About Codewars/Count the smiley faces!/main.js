countSmileys = (arr) => arr.filter(i => i.match(/^[:;][-~]?[)D]$/g) != null).length
