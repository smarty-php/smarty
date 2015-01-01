<?php
/**
 * Smarty Internal Plugin Resource File
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Uwe Tews
 * @author     Rodney Rehm
 */

/**
 * Smarty Internal Plugin Resource File
 * Implements the file system as resource for Smarty templates
 *
 * @package    Smarty
 * @subpackage TemplateResources
 */
class Smarty_Internal_Resource_File extends Smarty_Resource
{

    /**
     * build template filepath by traversing the template_dir array
     *
     * @param Smarty_Template_Source    $source    source object
     * @param  Smarty_Internal_Template $_template template object
     *
     * @return string fully qualified filepath
     * @throws SmartyException
     */
    protected function buildFilepath(Smarty_Template_Source $source, Smarty_Internal_Template $_template = null)
    {
        $file = str_replace(array('\\', '/./'), '/', $source->name);
        if ($source->isConfig) {
            $_directories = $source->smarty->getConfigDir();
        } else {
            $_directories = $source->smarty->getTemplateDir();
        }
        preg_match('#^((?P<absolute>[\/]|[a-zA-Z]:[\/])|(\[(?P<index>[^\]]+)\])|((?P<rel1>\.[\/])?(?P<rel2>(\.\.[\/])*))|(?P<skip>[\/]))?(?P<file>.+)$#', $file, $fileMatch);
        // go relative to a given template?
        if ($_template && $_template->parent instanceof Smarty_Internal_Template && (!empty($fileMatch['rel1']) || !empty($fileMatch['rel2']))) {
            if ($_template->parent->source->type != 'file' && $_template->parent->source->type != 'extends' && !$_template->parent->allow_relative_path) {
                throw new SmartyException("Template '{$file}' cannot be relative to template of resource type '{$_template->parent->source->type}'");
            }
            $path = dirname($_template->parent->source->filepath);
            if (!preg_match('/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/', $path)) {
                // the path gained from the parent template is relative to the current working directory
                // as expansions (like include_path) have already been done
                $path = getcwd() . '/' . $path;
            }
            // normalize path
            $path = str_replace(array('\\', './'), array('/', ''), $path);
            // simple relative
            if (!empty($fileMatch['rel1'])) {
                $file = $path . '/' . $fileMatch['file'];
            } else {
                for ($i = 1; $i <= substr_count($fileMatch['rel2'], '../'); $i ++) {
                    $path = substr($path, 0, strrpos($path, '/'));
                }
                $file = $path . '/' . $fileMatch['file'];
            }
            // files relative to a template only get one shot
            return $this->fileExists($source, $file) ? $file : false;
        }

        $_filepath = null;
        // template_dir index?
        if (!empty($fileMatch['index'])) {
            $index = $fileMatch['index'];
            $_directory = null;
            // try string indexes
            if (isset($_directories[$index])) {
                $_directory = $_directories[$index];
            } elseif (is_numeric($index)) {
                // try numeric index
                $index = (int) $index;
                if (isset($_directories[$index])) {
                    $_directory = $_directories[$index];
                } else {
                    // try at location index
                    $keys = array_keys($_directories);
                    $_directory = $_directories[$keys[$index]];
                }
            }
            if ($_directory) {
                $_filepath = $_directory . $fileMatch['file'];
                if ($this->fileExists($source, $_filepath)) {
                    return $_filepath;
                }
            }
        }

        // relative file name?
        if (empty($fileMatch['absolute'])) {
            foreach ($_directories as $_directory) {
                if (!empty($fileMatch['rel2'])) {
                    for ($i = 1; $i <= substr_count($fileMatch['rel2'], '../') + 1; $i ++) {
                        $_directory = substr($_directory, 0, strrpos($_directory, '/'));
                    }
                    $_filepath = $_directory . '/' . $fileMatch['file'];
                } else {
                    $_filepath = $_directory . $fileMatch['file'];
                }
                if ($this->fileExists($source, $_filepath)) {
                    return $_filepath;
                }
                if ($source->smarty->use_include_path && !preg_match('/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/', $_directory)) {
                    // try PHP include_path
                    if (function_exists('stream_resolve_include_path')) {
                        $_filepath = stream_resolve_include_path($_filepath);
                    } else {
                        $_filepath = Smarty_Internal_Get_Include_Path::getIncludePath($_filepath);
                    }

                    if ($_filepath !== false) {
                        if ($this->fileExists($source, $_filepath)) {
                            return $_filepath;
                        }
                    }
                }
            }
        } else {
            // try absolute filepath
            if ($this->fileExists($source, $file)) {
                return $file;
            }
        }
        // Could be relative to cwd
        if (empty($fileMatch['rel1'])) {
            $file = getcwd() . '/' . $fileMatch['file'];
        } else {
            $path = getcwd();
            for ($i = 1; $i <= substr_count($fileMatch['rel2'], '../'); $i ++) {
                $path = substr($path, 0, strrpos($path, '/'));
            }
            $file = $path . '/' . $fileMatch['file'];
        }
        if ($this->fileExists($source, $file)) {
            return $file;
        }

        // give up
        return false;
    }

    /**
     * test is file exists and save timestamp
     *
     * @param  Smarty_Template_Source $source source object
     * @param  string                 $file   file name
     *
     * @return bool                   true if file exists
     */
    protected function fileExists(Smarty_Template_Source $source, $file)
    {
        $source->timestamp = is_file($file) ? @filemtime($file) : false;
        return $source->exists = !!$source->timestamp;
    }

    /**
     * populate Source Object with meta data from Resource
     *
     * @param Smarty_Template_Source   $source    source object
     * @param Smarty_Internal_Template $_template template object
     */
    public function populate(Smarty_Template_Source $source, Smarty_Internal_Template $_template = null)
    {
        $source->filepath = $this->buildFilepath($source, $_template);

        if ($source->filepath !== false) {
            if (is_object($source->smarty->security_policy)) {
                $source->smarty->security_policy->isTrustedResourceDir($source->filepath);
            }

            $source->uid = sha1(getcwd() . $source->filepath);
            if ($source->smarty->compile_check && !isset($source->timestamp)) {
                $source->timestamp = @filemtime($source->filepath);
                $source->exists = !!$source->timestamp;
            }
        }
    }

    /**
     * populate Source Object with timestamp and exists from Resource
     *
     * @param Smarty_Template_Source $source source object
     */
    public function populateTimestamp(Smarty_Template_Source $source)
    {
        $source->timestamp = @filemtime($source->filepath);
        $source->exists = !!$source->timestamp;
    }

    /**
     * Load template's source from file into current template object
     *
     * @param  Smarty_Template_Source $source source object
     *
     * @return string                 template source
     * @throws SmartyException        if source cannot be loaded
     */
    public function getContent(Smarty_Template_Source $source)
    {
        if ($source->timestamp) {
            return file_get_contents($source->filepath);
        }
        if ($source instanceof Smarty_Config_Source) {
            throw new SmartyException("Unable to read config {$source->type} '{$source->name}'");
        }
        throw new SmartyException("Unable to read template {$source->type} '{$source->name}'");
    }

    /**
     * Determine basename for compiled filename
     *
     * @param  Smarty_Template_Source $source source object
     *
     * @return string                 resource's basename
     */
    public function getBasename(Smarty_Template_Source $source)
    {
        $_file = $source->name;
        if (($_pos = strpos($_file, ']')) !== false) {
            $_file = substr($_file, $_pos + 1);
        }

        return basename($_file);
    }
}
