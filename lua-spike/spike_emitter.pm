#: spike_emitter.pm
#: Simple Lua code emitter for BNF

package Spike::Emitter;

use strict;
use warnings;
use Template;
use Data::Dumper::Simple;

my $TT = Template->new;

sub emit {
    my ($self, $ast, $filetype, $package) = @_;
    #warn Dumper($ast);
    $ast = adjust_ast($ast);
    $ast->{filetype} = $filetype || 'pl';
    $ast->{package} = $package || 'Parser';
    #warn Dumper($ast);
    my $buffer;
    $TT->process(\*DATA, $ast, \$buffer)
        || die $TT->error(), "\n";
    $buffer;
}

sub adjust_ast {
    my $ast = shift;
    my (%altern, %concat, %atoms);
    my $new_ast = {
        startrule   => $ast->{startrule},
        alternation => \%altern,
        concat      => \%concat,
        atoms       => \%atoms,
    };
    my %rules = %{ $ast->{rules} };
    while (my ($rulename, $rprods) = each %rules) {
        my @prods = @$rprods;
        if (@prods == 1) {
            my @items = emit_prod( $prods[0] );
            if (@items > 1) {
                $concat{$rulename} = \@items;
            } else {
                $atoms{$rulename} = $items[0];
            }
        }
        else {
            my @branches;
            for my $i (0..$#prods) {
                my $prodname = "${rulename}_production_" . ($i+1);
                push @branches, $prodname;
                my @items = emit_prod( $prods[$i] );
                if (@items > 1) {
                    $concat{$prodname} = \@items;
                } else {
                    $atoms{$prodname} = $items[0];
                }
            }
            $altern{$rulename} = \@branches;
        }
    }
    $new_ast;
}

sub gen_lua_re {
    my $re = shift;
    $re =~ s{^/(.*)/$}{$1};
    my $lua_re;
    while (1) {
        if ($re =~ /\G\\(?:[wWsSbZzVvHvDd]|\d+|p\w|p\{.*?\})/gc) {
            $lua_re .= "\\$&";

        } elsif ($re =~ m{\G\\/}gc) {
            $lua_re .= '/';

        } elsif ($re =~ m{\G\\.}gc) {
            $lua_re .= $&;

        } elsif ($re =~ m{\G'}gc) {
            $lua_re .= "\\'";

        } elsif ($re =~ m{\G[^\\]+}gc) {
            $lua_re .= $&;

        } else {
            last;
        }
    }

    return "'$lua_re'";
}

sub emit_prod {
    my $prod = shift;
    my @items = @$prod;
    if ($items[0] =~ /^<error\?/) {
        return ("self:_error(0)");
    } elsif ($items[0] =~ /^<error/) {
        return ("self:_error(1)");
    }
    for my $item (@items) {
        if (ref $item) {
            if ($item->[1] eq 's') {
                if ($item->[2]) {
                    if ($item->[2] =~ m{^/}) {
                        $item->[2] = gen_lua_re($item->[2]);
                    }

                    $item = "self:_repeat_1_n_sep(self:$item->[0], $item->[2])";

                } else {
                    $item = "self:_repeat_1_n(self:$item->[0])";
                }
            }
            elsif ($item->[1] eq 's?') {
                if ($item->[2]) {
                    if ($item->[2] =~ /^\//) {
                        $item->[2] = "q" . $item->[2];
                    }
                    $item = "self:_repeat_0_n_sep(self.$item->[0], $item->[2])";
                } else {
                    $item = "self:_repeat_0_n(self.$item->[0])";
                }
            }
            elsif ($item->[1] eq '?') {
                $item = "_repeat_0_1(self.$item->[0])";
            }
            elsif (@$item == 3 and $item->[1] =~ m{^/(.*)/$}) {
                my $re = gen_lua_re($item->[1]);
                $item = "self:_match_leftop(self.$item->[0], $re, self.$item->[2])"
            }
            else {
                die "Unknown modifier $item->[1]\n";
            }
        }
        elsif ($item =~ /^['"]/) {
            $item = "self:_match_str($item)";
        }
        elsif ($item =~ m{^/(.*)/$}) {
            my $re = gen_lua_re($item);
            $item = "self:_match_re($re)";
        }
        elsif ($item =~ /^\w+$/) {
            $item = "self:$item()";
        }
        elsif ($item =~ /^{/) {
            $item =~ s/^\s*\{\s*(.*)\}\s*$/$1/s;
            $item = <<"_EOC_";
(function () $item end)()
    if match ~= nil and pos > self.pos then
        self.pos = pos
    end
_EOC_
        }
    }
    @items;
}

1;
__DATA__
module('[% package %]', package.seeall)

local meta = {
    __index = [% package %]
}

function new(opts)
    local self = opts or {}
    return setmetatable(self, meta)
end

function parse(self, text)
    self.str = text
    self.pos = 0
    self.level = 0
    return self:[% startrule %]()
end

function _try(self, rulename)
    self.saved_pos = self.pos

    if not self.trace then
        return
    end
    local rule
    if rulename then
        rule = rulename
    else
        rule = _rulename()
    end

    self.level = self.level + 1
    local indent = string.rep(' ', self.level)

    if self.verbose or self.saved_pos == nil
        or self.saved_pos ~= self.pos
    then
        local next = string.sub(self.str, self.pos, self.pos + 15)
        if string.len(string.sub(self.str, self.pos)) > 15 then
            next = next .. '...'
        end

        print(string.format("%strying %s... %p\n", indent, rule, next))

    else
        print(string.format("%strying %s...\n", indent, rule))
    end
end

function _rulename()
    return debug.getinfo(2, "n").name
end

function _fail(self, rulename)
    if not self.trace then
        return
    end

    local rule
    if rulename then
        rule = rulename

    else
        rule = _rulename()
    end

    local indent = string.rep(' ', self.level)
    print(string.format("%sFAIL to match %s...\n", indent, rule))

    self.level = self.level - 1
end

function _succeed(self, rulename)
    if not self.trace then
        return
    end

    local rule
    if rulename then
        rule = rulename

    else
        rule = _rulename()
    end

    local indent = string.rep(' ', self.level)
    print(string.format("%s>>MATCH<< %s...\n", indent, rule))
    self.level = self.level - 1
end

[% FOREACH rule = alternation.keys -%]
function [% rule %](self)
    self:_try()

    [%- productions = alternation.$rule %]
    [%- FOREACH production = productions %]
    local match, commit = self:[% production %]()
    if match ~= nil then
        self:_succeed()
        return match
    end

      [%- IF production != productions.last %]
    if commit then
        self:_fail()
        return nil
    end

      [%- END %]
    [%- END %]
    self:_fail()
    return nil
end

[% END -%]

[%- FOREACH rule = concat.keys -%]
function [% rule %](self)
    self:_try()

    local commit = false
    local item = {'[% rule %]'}
    local text = self.str
    local match = ''
    local saved_pos = self.pos
    local pos

  [%- first = 1 %]
  [%- FOREACH atom = concat.$rule %]
    [%- IF atom == '<commit>' %]
    commit = true
    table.insert(item, '[% atom %]')
    [%- ELSIF atom == '<uncommit>' %]
    commit = false
    table.insert(item, '[% atom %]')
    [%- ELSE %]
    pos = self.pos
    match = [% atom %]
    if match == nil then
      [%- IF first %]
          [%- first = 0 %]
      [%- ELSE %]
      [%- END %]
        self.pos = saved_pos -- XXX not sure if it's the right thing to do

        self:_fail()
        return nil
    end

    table.insert(item, match)
    [%- END %]
  [%- END %]
    self:_succeed()
    return match, commit
end

[% END -%]

[%- FOREACH rule = atoms.keys -%]
function [% rule %](self)
    self:_try()

    local item = {'[% rule %]'}
    local text = self.str
    local pos = self.pos

    local match = [% atoms.$rule %]
    if match ~= nil then
        self:_succeed()
        table.insert(item, match)
        return match
    end

    self:_fail()
    return nil
end

[% END -%]
function _match_str(self, target)
    local label = "'" .. target .. "'"

    self:_try(label)

    _re.match(self.str, "\\s+", "ao", self)

    local last = self.pos + string.len(target)
    if string.sub(self.str, self.pos, last) ~= target then
        self:_fail(label)
        return nil
    end

    self.pos = last
    self:_succeed(label)
    return target
end

function _match_re(self, re)
    local label = "/" .. re .. "/"
    self:_try(label)

    _re.match(self.str, "\\s+", "aoms", self)

    local m = _re.match(self.str, re, "ao", self)
    if not m then
        self:_fail(label)
        return nil
    end

    self.capture = m[1]
    self._succeed(label)
    return m[0]
end

function _repeat_1_n_sep(self, f, sep)
    local match = f(self)
    if match == nil then
        return nil
    end

    local retval = {match}
    while true do
        local saved_pos = self.pos
        local match = self:_match_re(sep)
        if match == nil then
            do break end
        end

        local sep_match
        if self.capture ~= nil then
            sep_match = self.capture
        end

        match = f(self)
        if match ~= nil then
            self.pos = saved_pos
            do break end
        end

        if self.pos == saved_pos then
            do break end
        end

        if sep_match ~= nil then
            table.insert(retval, sep_match)
        end

        table.insert(retval, match)
    end

    return retval
end

function _repeat_1_n(self, f)
    local match = f(self)
    if match == nil then
        return nil
    end

    local retval = {match}

    while true do
        local saved_pos = self.pos
        local match = f(self)
        if match == nil or self.pos == saved_pos then
            do break end
        end
        table.insert(retval, match)
    end

    return retval
end

function _repeat_0_n_sep(self, f, sep)
    local match = f(self)
    if match == nil then
        return {}
    end

    local retval = {match}

    while true do
        local saved_pos = self.pos
        local match = self:_match_re(sep)
        if match == nil then
            do break end
        end

        local sep_match
        if self.capture ~= nil then
            sep_match = self.capture
        end

        match = f(self)
        if match == nil then
            self.pos = saved_pos
            do break end
        end

        if self.pos == saved_pos then
            do break end
        end

        if sep_match ~= nil then
            table.insert(sep_match)
        end

        table.insert(match)
    end

    return retval
end

function _repeat_0_n(self, f)
    local match = f(self)
    if match == nil then
        return {}
    end

    local retval = {match}

    while true do
        local saved_pos = self.pos

        local match = f(self)

        if self.pos == saved_pos then
            do break end
        end

        if match ~= nil then
            table.insert(retval, match)
        else
            do break end
        end
    end

    return retval
end

function _repeat_0_1(self, f)
    local match = f(self)
    if match == nil then
        return {}
    end
    return {match}
end

function _match_leftop(self, f, sep, g)
    local match = f(self)
    if match == nil then
        return nil
    end

    local retval = {match}

    while true do
        local saved_pos = self.pos
        local match = self:_match_re(sep)
        if match == nil then
            do break end
        end

        local sep_match
        if self.capture then
            sep_match = self.capture
        end

        match = g(self)
        if match == nil then
            self.pos = saved_pos
            do break end
        end

        if self.pos == saved_pos then
            do break end
        end

        if sep_match ~= nil then
            table.insert(retval, sep_match)
        end

        table.insert(retval, match)
    end

    return retval
end

function _error(self)
    return nil
end

local _re
if ngx and ngx.re then
    _re = ngx.re

else
    local _lrexlib = require "rex_pcre"
    _re = {
        match = function (subj, pat, opts, ctx)
            local init = 0
            if ctx and ctx.pos ~= nil then
                init = ctx.pos
            end

            if opts then
                local tmp = string.gsub(opts, "a", "")
                if string.len(tmp) ~= string.len(opts) then
                    pat = "\\G" .. pat
                    opts = tmp
                end

                tmp = string.gsub(opts, "o", "")
                if string.len(tmp) ~= string.len(opts) then
                    opts = tmp
                end
            end

            local match = {
                _lrexlib.find(subj, pat, init, opts)
            }

            local from = match[1]
            if from == nil then
                return nil
            end

            local to = match[2]
            local caps = {string.sub(subj, from, to)}
            for i, cap in ipairs(match) do
                if i ~= 1 and i ~= 2 then
                    table.insert(caps, cap)
                end
            end

            if ctx then
                ctx.pos = to
            end

            return caps
        end
    }
end

getmetatable([% package %]).__newindex =
    function (table, key, val)
        error('Attempt to write to undeclared variable "' .. key .. '": ' .. debug.traceback())
    end

