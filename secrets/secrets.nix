let
  me =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAE4m/t+pICC5eVU2kh0BtuSeBT3RGtNam/fRyB6jlsO";
in {
  "ovpn.age".publicKeys = [ me ];
  age.secrets.ovpn.file = ./ovpn.age
}
