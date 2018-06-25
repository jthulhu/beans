class CustomDict:
    def __init__(self, dict_ = {}):
        self.dict = dict_
    def __setitem__(self, key, value):
        if key in self.dict.keys():
            for element in dir(value):
                if not (element.startswith("__") or element.endswith("__")):
                    self.dict[key].__setattr__(element, value.__getattribute__(element))
        else:
            self.dict[key] = value
    def __delitem__(self, key):
        del self.dict[key]
    def __getitem__(self, key):
        return self.dict[key]

class ItemProxy:
    def __init__(self, dict_, key):
        self.dict = dict_
        self.key = key
    def __call__(self, *args, **kwargs):
        return self._get_item()(*args, **kwargs)
    def __getattr__(self, attribute):
        return object.__getattribute__(self, "_get_item")().__getattribute__(attribute)
    def _get_item(self):
        return self.dict.dict[self.key]
    def __repr__(self):
        return str(self._get_item())
    
class CustomDict2:
    def __init__(self, dict_={}):
        self.dict = dict_
    def __getitem__(self, key):
        return ItemProxy(self, key)
    def __delitem__(self, key):
        del self.dict[key]
    def __setitem__(self, key, value):
        self.dict[key] = value
    def __repr__(self):
        return str(self.dict)
        
            
class CD3(dict):
    def __getitem__(self, key):
        return ItemProxy(self, key)
