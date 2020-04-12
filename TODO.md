# Todo

## Add unit test for strip issue in correct branch
tests/UnitTests/TemplateSource/TagTests/Strip/CompileStripTest.php 
```
@@ -76,6 +76,7 @@ class CompileStripTest extends PHPUnit_Smarty
                      array("{'Var'}\n <b></b> <c></c>", 'Var<b></b> <c></c>', '', $i ++),
                      array("\n<b></b>  <c></c>", '<b></b> <c></c>', '', $i ++),
                      array("\n<b></b>\n  <c></c>", '<b></b><c></c>', '', $i ++),
+                     array("\n<b>\n  {* a comment *}\n   <c>", '<b><c>', '', $i ++),

         );
     }
```

## Add unit test for isset issue in correct branch
tests/UnitTests/TemplateSource/ValueTests/PHPfunctions/PhpFunctionTest.php
```php
    /**
     * test PHP isset() on (non-)variables
     * @dataProvider        dataTestIsset3
     * @param string $strTemplate template to test
     * @param string $result expected result
     */
    public function testIsset3($strTemplate, $result)
    {
        $this->smarty->disableSecurity();

        $this->smarty->assign('varobject', new TestIsset());
        $this->smarty->assign('vararray', $vararray = [
            'keythatexists' => false,
            'keywitharray' => [1 => 1],
            'keywithobject' => new TestIsset()]
        );

        $this->smarty->assign('key', 'A');
        $this->smarty->assign('_varsimpleA', 1);
        $this->smarty->assign('varsimpleB', 0);
        $this->smarty->assign('varsimpleC', null);

        $this->assertEquals($result, $this->smarty->fetch('string:' . $strTemplate));
    }

    /**
     * Data provider for testIsset3
     */
    public function dataTestIsset3()
    {
        return array(
            array('{if isset($varobject->arr)}true{else}false{/if}', 'true'),
            array('{if isset($vararray["keywitharray"])}true{else}false{/if}', 'true'),
            array('{if isset($vararray["keythatexists"])}true{else}false{/if}', 'true'),
            array('{if isset($vararray["nonexistingkey"])}true{else}false{/if}', 'false'),
            array('{if isset($_GET["sscr6hr6cz34j6"])}true{else}false{/if}', 'false'),
            array('{if isset(count([\'hi\']))}true{else}false{/if}', 'true'),
            array('{if isset($vararray[\'keywitharray\'][intval(\'1\')])}true{else}false{/if}', 'true'),
            array('{if isset($vararray[\'keywithobject\']->arr[\'isSet\'])}true{else}false{/if}', 'true'),
            array('{if isset($vararray[\'keywithobject\']->arr[\'isNull\'])}true{else}false{/if}', 'false'),
            array('{if isset($varobject->arr[\'isSet\'])}true{else}false{/if}', 'true'),
            array('{if isset($varobject->arr[\'isNull\'])}true{else}false{/if}', 'false'),
            array('{if isset($_varsimpleA)}true{else}false{/if}', 'true'),
            array('{if isset($varsimpleB)}true{else}false{/if}', 'true'),
            array('{if isset($varsimpleC)}true{else}false{/if}', 'false'),
            array('{if isset($_varsimpleA && varsimpleB)}true{else}false{/if}', 'true'),
            array('{if isset($_varsimpleA && varsimpleC)}true{else}false{/if}', 'true'),
            array('{if isset($_varsimple{$key})}true{else}false{/if}', 'true'),
        );
    }
```