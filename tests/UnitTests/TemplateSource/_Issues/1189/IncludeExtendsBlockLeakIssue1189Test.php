<?php
/**
 * Smarty PHPunit test reproducing issue #1189.
 *
 * When a parent template is {include}d, then a child template that {extends}
 * the parent overrides a {block}, a subsequent {include} of the parent in the
 * same render must still show the parent's block content.
 *
 * The block override from the extending child must not leak into the later
 * include of the parent template.
 *
 * @see https://github.com/smarty-php/smarty/issues/1189
 *
 * @preserveGlobalState    disabled
 */
class IncludeExtendsBlockLeakIssue1189Test extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    /**
     * Sequence: include parent -> include child(extends parent) -> include parent.
     * Expected: PARENT CHILD PARENT
     * Bug (#1189): PARENT CHILD CHILD
     */
    public function testBlockOverrideDoesNotLeakIntoLaterParentInclude()
    {
        $result = $this->smarty->fetch('top.tpl');
        $this->assertSame('PARENT CHILD PARENT', preg_replace('/\s+/', ' ', trim($result)));
    }
}
