\$config\_overwrite {#variable.config.overwrite}
===================

If set to TRUE, the default then variables read in from 
[config files](../../designers/config-files.md) will overwrite each other. Otherwise, the
variables will be pushed onto an array. This is helpful if you want to
store arrays of data in config files, just list each element multiple
times.

This examples uses [`{cycle}`](../../designers/language-custom-functions/language-function-cycle.md) to output a
table with alternating red/green/blue row colors with
`$config_overwrite` = FALSE.

The config file.


    # row colors
    rowColors = #FF0000
    rowColors = #00FF00
    rowColors = #0000FF

        

The template with a [`{section}`](../../designers/language-builtin-functions/language-function-section.md) loop.


    <table>
      {section name=r loop=$rows}
      <tr bgcolor="{cycle values=#rowColors#}">
        <td> ....etc.... </td>
      </tr>
      {/section}
    </table>

        

See also [`{config_load}`](../../designers/language-builtin-functions/language-function-config-load.md),
[`getConfigVars()`](../../programmers/api-functions/api-get-config-vars.md),
[`clearConfig()`](../../programmers/api-functions/api-clear-config.md), 
[`configLoad()`](../../programmers/api-functions/api-config-load.md)
and the [config files section](../../designers/config-files.md).
