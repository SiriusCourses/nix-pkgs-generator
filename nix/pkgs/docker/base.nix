{ pkgs
, glibcLocales-light
}:
let config_env = [
      "PATH=/bin"
      "LOCALE_ARCHIVE=${glibcLocales-light}/lib/locale/locale-archive"
      "LC_ALL=en_US.UTF-8"
      "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
    ];
in pkgs.dockerTools.buildImage {
  name = "sirius-base";
  contents = [ pkgs.busybox pkgs.iana-etc pkgs.tini pkgs.cacert pkgs.gettext ];
  config.Env = config_env;
} // { passthru = { configEnv = config_env; }; }
