Resources {#plugins.resources}
=========

Resource plugins are meant as a generic way of providing template
sources or PHP script components to Smarty. Some examples of resources:
databases, LDAP, shared memory, sockets, and so on.

Custom Resources may be put in a file `resource.foobarxyz.php` within
your [`$plugins_dir`](#variable.plugins.dir), or registered on runtime
with [`registerResource()`](#api.register.resource). In either case you
will be able to access that resource by prepending its name to the
template you\'re addressing: `foobarxyz:yourtemplate.tpl`.

If a Resource\'s templates should not be run through the Smarty
compiler, the Custom Resource may extend `\Smarty\Resource\UncompiledPlugin`.
The Resource Handler must then implement the function
`renderUncompiled(\Smarty\Template $_template)`. `$_template` is
a reference to the current template and contains all assigned variables
which the implementor can access via
`$_template->getSmarty()->getTemplateVars()`. These Resources simply echo
their rendered content to the output stream. The rendered output will be
output-cached if the Smarty instance was configured accordingly. See
`src/Resource/PhpPlugin.php` for an example.

If the Resource\'s compiled templates should not be cached on disk, the
Custom Resource may extend `\Smarty\Resource\RecompiledPlugin`. These Resources
are compiled every time they are accessed. This may be an expensive
overhead. See `src/Resource/StringEval.php` for an
example.


    <?php

    use Smarty\Smarty;

    /**
     * MySQL Resource
     *
     * Resource Implementation based on the Custom API to use
     * MySQL as the storage resource for Smarty's templates and configs.
     *
     * Table definition:
     * <pre>CREATE TABLE IF NOT EXISTS `templates` (
     *   `name` varchar(100) NOT NULL,
     *   `modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
     *   `source` text,
     *   PRIMARY KEY (`name`)
     * ) ENGINE=InnoDB DEFAULT CHARSET=utf8;</pre>
     *
     * Demo data:
     * <pre>INSERT INTO `templates` (`name`, `modified`, `source`) VALUES ('test.tpl', "2010-12-25 22:00:00", '{$x="hello world"}{$x}');</pre>
     *
    
     * @author Rodney Rehm
     */
    class My_Resource_Mysql extends \Smarty\Resource\CustomPlugin {
        // PDO instance
        protected $db;
        // prepared fetch() statement
        protected $fetch;
        // prepared fetchTimestamp() statement
        protected $mtime;

        public function __construct() {
            try {
                $this->db = new PDO("mysql:dbname=test;host=127.0.0.1", "smarty", "smarty");
            } catch (PDOException $e) {
                throw new \Smarty\Exception('Mysql Resource failed: ' . $e->getMessage());
            }
            $this->fetch = $this->db->prepare('SELECT modified, source FROM templates WHERE name = :name');
            $this->mtime = $this->db->prepare('SELECT modified FROM templates WHERE name = :name');
        }
        
        /**
         * Fetch a template and its modification time from database
         *
         * @param string $name template name
         * @param string $source template source
         * @param integer $mtime template modification timestamp (epoch)
         * @return void
         */
        protected function fetch($name, &$source, &$mtime)
        {
            $this->fetch->execute(array('name' => $name));
            $row = $this->fetch->fetch();
            $this->fetch->closeCursor();
            if ($row) {
                $source = $row['source'];
                $mtime = strtotime($row['modified']);
            } else {
                $source = null;
                $mtime = null;
            }
        }
        
        /**
         * Fetch a template's modification time from database
         *
         * @note implementing this method is optional. Only implement it if modification times can be accessed faster than loading the comple template source.
         * @param string $name template name
         * @return integer timestamp (epoch) the template was modified
         */
        protected function fetchTimestamp($name) {
            $this->mtime->execute(array('name' => $name));
            $mtime = $this->mtime->fetchColumn();
            $this->mtime->closeCursor();
            return strtotime($mtime);
        }
    }


    
    $smarty = new Smarty();
    $smarty->registerResource('mysql', new My_Resource_Mysql());

    // using resource from php script
    $smarty->display("mysql:index.tpl");
    ?>

         

And from within Smarty template:


    {include file='mysql:extras/navigation.tpl'}

         

See also [`registerResource()`](#api.register.resource),
[`unregisterResource()`](#api.unregister.resource).
