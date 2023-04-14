{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
{
  https://opensource.apple.com/source/xnu/xnu-792/osfmk/mach/mach_time.h.auto.html
}
unit MachTime;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$linkframework CoreFoundation}

interface

type
  mach_timebase_info_data_t = record
    numer: UInt32;
    denom: UInt32;
  end;
  mach_timebase_info_t = ^mach_timebase_info_data_t;

function mach_absolute_time: UInt64; cdecl; external name 'mach_absolute_time';
function mach_timebase_info(info: mach_timebase_info_t): Integer; cdecl; external name 'mach_timebase_info';

implementation

end.
