<?php

/**
 * Config_File class.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * You may contact the author of Config_File by e-mail at:
 * {@link andrei@php.net}
 *
 * The latest version of Config_File can be obtained from:
 * http://smarty.php.net/
 *
 * @link http://smarty.php.net/
 * @version 2.6.1-dev
 * @copyright Copyright: 2001-2003 ispi of Lincoln, Inc.
 * @author Andrei Zmievski <andrei@php.net>
 * @access public
 * @package Smarty
 */

/* $Id$ */

/**
 * Config file reading class
 * @package Smarty
 */
class Config_File {
    /**#@+
     * Options
     * @var boolean
     */
    /**
     * Controls whether variables with the same name overwrite each other.
     */
    var $overwrite        =    true;

    /**
     * Controls whether config values of on/true/yes and off/false/no get
     * converted to boolean values automatically.
     */
    var $booleanize        =    true;

    /**
     * Controls whether hidden config sections/vars are read from the file.
     */
    var $read_hidden     =    true;

    /**
     * Controls whether or not to fix mac or dos formatted newlines.
     * If set to true, \r or \r\n will be changed to \n.
     */
    var $fix_newlines =    true;
    /**#@-*/

    /** @access private */
    var $_config_path    = "";
    var $_config_data    = array();
    /**#@-*/

    /**
     * Constructs a new config file class.
     *
     * @param string $config_path (optional) path to the config files
     */
    function Config_File($config_path = NULL)
    {
        if (isset($config_path))
            $this->set_path($config_path);
    }


    /**
     * Set the path where configuration files can be found.
     *
     * @param string $config_path path to the config files
     */
    function set_path($config_path)
    {
        if (!empty($config_path)) {
            if (!is_string($config_path) || !file_exists($config_path) || !is_dir($config_path)) {
                $this->_trigger_error_msg("Bad config file path '$config_path'");
                return;
            }
            if(substr($config_path, -1) != DIRECTORY_SEPARATOR) {
                $config_path .= DIRECTORY_SEPARATOR;
            }

            $this->_config_path = $config_path;
        }
    }


    /**
     * Retrieves config info based on the file, section, and variable name.
     *
     * @param string $file_name config file to get info for
     * @param string $section_name (optional) section to get info for
     * @param string $var_name (optional) variable to get info for
     * @return string|array a value or array of values
     */
    function &get($file_name, $section_name = NULL, $var_name = NULL)
    {
        if (empty($file_name)) {
            $this->_trigger_error_msg('Empty config file name');
            return;
        } else {
            $file_name = $this->_config_path . $file_name;
            if (!isset($this->_config_data[$file_name]))
                $this->load_file($file_name, false);
        }

        if (!empty($var_name)) {
            if (empty($section_name)) {
                return $this->_config_data[$file_name]["vars"][$var_name];
            } else {
                if(isset($this->_config_data[$file_name]["sections"][$section_name]["vars"][$var_name]))
                    return $this->_config_data[$file_name]["sections"][$section_name]["vars"][$var_name];
                else
                    return array();
            }
        } else {
            if (empty($section_name)) {
                return (array)$this->_config_data[$file_name]["vars"];
            } else {
                if(isset($this->_config_data[$file_name]["sections"][$section_name]["vars"]))
                    return (array)$this->_config_data[$file_name]["sections"][$section_name]["vars"];
                else
                    return array();
            }
        }
    }


    /**
     * Retrieves config info based on the key.
     *
     * @param $file_name string config key (filename/section/var)
     * @return string|array same as get()
     * @uses get() ret       ntain/ecu9///
cross mulget() ret       ntain
u=rw,g=r,o=r
12243
<?ain

/**
 * out compilin:
 * h
 *
 * T
      ; if    fass bofts"]e; yublic
 ed ithout e. (Mohis/fig_*------f   - aei Zth no* @is lith nGNU Lspei ZGer wadre- aicg_*-Lg_path) add- aisay c_dath nFass Softs"]eame ae     ; e!emat
  ke - adde {
s lith nLg_path_coscr  iyubAndibuteo_herin/

$th - addh
 *
 * T
      ; if     ithout e.che_ith nhing Bad c  -wulz,d debuy_rent*   t WITHOUT ANY  vaRANTY; (!ema    (Anith npathion_s"]rect    nt* MERCHANTABILITYcoscFITNESS FempA erag iULAR PURPOSE.  Sss th nGNUg_*-Lspei ZGer wadre- aic-Lg_path)dataiablede    if
 *
 * Publ ConfigMonle_ncompiVerilep    mth nGNU Lspei ZGer wadre- aicg_*-Lg_path)  iss (!emp-
      ; if;t thig_seronte,ersionnFass Softs"]eg_*-me ae     , Inc wi59 Ttruc
  hing, Sunte,33onsBforire (A  02111-1307  USA
 *
 * Publm->_     g in te  hhata  mor messages _dae-ates   :g_*-{@o synhisins@ain.netove*
 * T
    win    - adde  mor messages  - initob   riVe
 * :g_*-ixed:/ get()
tain.net/ve*
 * @o synixed:/ get()
tain.net/ve* @e - adde {6.1-r og_*-@lep titiesCep titie:m 2.1- 2.don'nto  mLeparlre Inc g_*-@  hhataror_ms Zmievif} <hisins@ain.net>g_*-@ Monte)d- aicg_*-@nd k pat     (Mo @r
/* $Id$secju Exp ma @r
/**
 * out coARRANT). (Andrtch.php*-@nd k pat     (Mo @r}
 *
 or messages { $fil/**#@+file_n* Oibute file_n* @    Monte)
file_n*/vefil/**file_n* or s, c contematonfiguratiop-level on_ext  - ims (Monte,   - n)
  ctions" @refil    $ms (Monte,   - fmpiiiie qe;n 1.5./**file_n* or s, c contematooblem witto _pditin/e qe/y an
   onf/ attr/ buint objec*_   ribution (Monte)
_config_  ht
   am syctions" @refil    $s for faste   - fmpiiiie qe;n 1.5./**file_n* or s, c contematoixed problem w
-------/ting ve Smacluunction
 R A Pfile_n*/vefil    $ bug ixed pr- fmpiiiie qe;n 1.5./**file_n* or s, c contemato    -mentec.
 maccoscd, Me {
  onte)ew
   if
 le_n* I noame/rsi qe, \ta r \t\n-wulz,d duld have o \dh
 le_n*@refil    $e Soescape mopiiiie qe;n$fil/**#@- @r
$fil/**-@ Monte)driv/

 *@refil    $tra and or me fmpi"";refil    $tra and e  ae fmpit_temp);refil/**#@- @r
$fil/**file_n* or g withe)
 ne. oblem wRRANT:
 * h
 le_n*file_n* @11, 20nte)
  efault tor me(&get($fi) and
 ersionnoblem wRRAN file_n*@refilfixed indor messages(efault tor me= NULLs.php,{ $filllll th(   et(efault tor m)s.php,,,,,,,,,es
     ettion.(efault tor m);refil}
n 1.5./**file_n* Same/ith id
 onte Soblem eleconsol
     - initers, h
 le_n*file_n* @11, 20nte)
  efault tor meand
 ersionnoblem wRRAN file_n*@refilfixed ind ettion.(efault tor m).php,{ $filllll th(!ion i(efault tor m)s,{ $filllllllll th(!ionnte)
 (efault tor m) || ! doll fixed efault tor m) || !ionams efault tor m)),{ $fillllllllllllles
    _te)t_da If se_msg("Bcluoblem wRRANTand
 'efault tor m'");refilphp,,,,,,,,,h '$co;refillllllllloved licilllll t(ed.nte efault tor m,_Co) wit (MECTORY_SEeraATOR),{ $filllllllllllllea bug iand
 .it (MECTORY_SEeraATOR;refilllllllllov.php,,,,,,,,,es
    tra and or me=default tand
;refillllloved l}
n 1.5./**file_n* Rete) (Ae)
    - 
    ra nte    nside ls"]les ca

Vers variable. (Ah
 le_n*file_n* @11, 20nte)
  e doll  - ioblem wRRANTerssign
    al settin*-@nd, 20nte)
  e]les cal  - i(&get($fi) nction serssign
    al settin*-@nd, 20nte)
  e val  - i(&get($fi)  variableerssign
    al settin*-@)
     nte)
 |his->_a_confia r his->_ditconfig
 le_n*@refilfixed ind&sig($ doll  -  de]les cal  - i= NULL de val  - i= NULLs.php,{ $filllll th(ion i(e doll  - )),{ $fillllllllles
    _te)t_da If se_msg('Eon instead oRRANT  - ');refilphp,,,,,h '$co;refilllll} (Mth){ $fillllllllle doll  - i=des
    tra and or me.le doll  - ;refilllllllll th(!io et(es
    tra and e  a[e doll  - ])s.php,,,,,,,,,lllles
    ny proble($ doll  -  d attr);refilphp,}
n$filllll th(!ion i(e val  - )),{ $filllllllll th(ion i(e]les cal  - )),{ $filllllllll$fil)
     es
    tra and e  a[e doll  - ]["ting"][e val  - ];refilllllllllo (Mth){ $filllllllllllll t(   et(es
    tra and e  a[e doll  - ][maluoted
"][e]les cal  - ]["ting"][e val  - ])s.php,,,,,,,,,llll$fil)
     es
    tra and e  a[e doll  - ]["aluoted
"][e]les cal  - ]["ting"][e val  - ];refilllllllll$fil(Mth.php,,,,,,,,,$filllllrty.cons_temp);refillllllllloved licil} (Mth){ $filllllllll th(ion i(e]les cal  - )),{ $filllllllll$fil)
     (his->)es
    tra and e  a[e doll  - ]["ting"];refilllllllllo (Mth){ $filllllllllllll t(   et(es
    tra and e  a[e doll  - ][maluoted
"][e]les cal  - ]["ting"])s.php,,,,,,,,,llll$fil)
     (his->)es
    tra and e  a[e doll  - ]["aluoted
"][e]les cal  - ]["ting"];refilllllllll$fil(Mth.php,,,,,,,,,$filllllrty.cons_temp);refillllllllloved licil}ved l}
n 1.5./**file_n* Rete) (Ae)
    - 
    ra nte    nsi[$fi
 le_n*file_n* @11, 20e doll  - inte)
  oblem w)
  (dwrap an/aluoted/tins.php,,*-@)
     nte)
 |his->_n_ext adsig(s.php,,*-@ebuadsig(sl)
 e) (Ae)lse {
      unctioblem wRRANTVersh '$conc  
 le_n*@refilfixed ind&sig_)
  efault t)
 s.php,{ $filllllbal ($ doll  -  de]les cal  -  de val  - )mpip. l
  ('/)

efault t)
 , 3);refilphp,id. rrampi&es
    sig($ doll  -  de]les cal  -  de val  - );refilllll)
     ed. rra;refil}
nllll/**file_n* G /**lz,ge useioblem wRRANTion ai
 le_n*file_n* @rty.cons_temnst.FOO (Mditge useioblem wRRANTion a
 le_n*@refilfixed indds w doll  - ion.php,{ $filllllrty.cons_tem_)
 s(es
    tra and e  a);refil}
n 1.5./**file_n* G /**lz,nction s. (Andunctiatge useiR A Pfile_n*file_n* @11, 20nte)
  e doll  - ioblem wRRANTerssignnction s. (Andunctfile_n* @rty.cons_temnst.FOO (Mditnction s. (Andunctio forlate blck fun
 le_n*@refilfixed indds w]les cal  - s(e doll  - ).php,{ $fillllle doll  - i=des
    tra and or me.le doll  - ;refilllll th(!io et(es
    tra and e  a[e doll  - ])s){ $fillllllllles
    tte)t_da If se_msg("Unops wioblem wRRANT'e doll  - '");refilphp,,,,,h '$co;refilllll}
n$filllllrty.cons_tem_)
 s(es
    tra and e  a[e doll  - ][maluoted
"]);refil}
n 1.5./**file_n* G /**lz,U Lessemarty
conso variable. (Anh
 le_n*file_n* @11, 20nte)
  e doll  - ioblem wRRANTerssign
    al settin*-@nd, 20nte)
  e]les cal  - i(&get($fi) nction serssign
    al settin*-@rty.cons_temnst.FOO (Mditnfiguratio. (Andunctio forlate blck fun/aluoted
 le_n*@refilfixed indds w val  - s(e doll  -  de]les campiNULLs.php,{ $filllll th(ion i(e doll  - )),{ $fillllllllles
    _te)t_da If se_msg('Eon instead oRRANT  - ');refilphp,,,,,h '$co;refilllll} (Mth) th(!io et(es
    tra and e  a[e doll  - ])s){ $fillllllllles
    tte)t_da If se_msg("Unops wioblem wRRANT'e doll  - '");refilphp,,,,,h '$co;refilllll}
n$filllll th(ion i(e]les ca)s.php,,,,,,,,,rty.cons_tem_)
 s(es
    tra and e  a[e doll  - ][mting"]);refilphp,(Mth.php,,,,,,,,,rty.cons_tem_)
 s(es
    tra and e  a[e doll  - ][maluoted
"][e]les ca]["ting"]);refil}
n 1.5./**file_n* Cmenttge useioblem we  aeocume ctho  reRRANTcumelz,l
   h
 le_n*file_n* @11, 20nte)
  e doll  - iRRANTY; opertioblem we  aeocu
 le_n*@refilfixed indopert(e doll  - mpiNULLs.php,{ $filllll th(e doll  - i=   NULLs.php,,,,,,,,,es
    tra and e  aepit_temp);refil$fil(Mthl th(   et(es
    tra and e  a[e doll  - ])s.php,,,,,,,,,es
    tra and e  a[e doll  - ]mpit_temp);refil}
n 1.5./**file_n* Le uerilelem eleconsol
  ABILITYsyctions" file_n* @11, 20nte)
  e doll  - iRRANT  - /**
  a$file_n*-@nd, 20Monte)
_$s. (Andtion.contematon time.nstead of fal Configbn
 le_n*efilllllllllefilllllllllefillls. (Andrei)
  nside l. (A
 le_