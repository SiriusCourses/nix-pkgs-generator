self:
  let glibcLocales-light = self.glibcLocales.override {
       allLocales = false;
       locales = [
         "en_US.UTF-8/UTF-8"
         "ru_RU.UTF-8/UTF-8"
         ];
       };
  in { sirius-base = self.callPackage ./pkgs/docker/base.nix { glibcLocales-light = glibcLocales-light; };
       # add docker images here
     }
