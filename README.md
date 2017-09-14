# hal

A really simple Hal+Json library for Erlang

This is my first attempt at creating a very basic Erlang/OTP library application
to generate Hal+Json data structures.

# Setting Up

You will need to following installed:

- [Vagrant](https://www.vagrantup.com)
- [Ansible](https://www.ansible.com/)

Navigation to the `/tools/vagrant` directory and run the following command.

```bash
$ vagrant up
```

After completion, you should be able to SSH into the VM using the following command.

```bash
$ vagrant ssh
```

Then to verify that Erlang is installed, you can run the `erl` command

```bash
$ erl
```

To exit the VM simply

```bash
$ exit
```

Once, you have `SSH`ed into the VM. You can navigate to the project directory.

```bash
$ cd /mnt/project
```

## Running Eunit Tests

```bash
$ ./rebar3 eunit
```

## Generating Documentation

```bash
$ ./rebar3 edoc
```
