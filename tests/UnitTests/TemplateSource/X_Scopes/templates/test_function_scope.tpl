{strip}
    {function 'a'}
        {call 'b'}
    {/function}
{function 'b'}
    {include 'test_function_scope_include.tpl'}
{/function}
{function c}
    {call $scope}
{/function}
{function 'none'}
    {$foo = 'newvar'}
    {checkvar var=foo}
{/function}
{function 'local'}
    {$foo = 'newvar' scope='local'}
    {checkvar var=foo}
{/function}
{function 'parent'}
    {$foo = 'newvar' scope='parent'}
    {checkvar var=foo}
{/function}
{function 'tpl_root'}
    {$foo = 'newvar' scope='tpl_root'}
    {checkvar var=foo}
{/function}
{function 'smarty'}
    {$foo = 'newvar' scope='smarty'}
    {checkvar var=foo}
{/function}
{function 'global'}
    {$foo = 'newvar' scope='global'}
    {checkvar var=foo}
{/function}
{call 'a'}