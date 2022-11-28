######## scripts for creating SQL queries from ODK XLSForms
library(stringr)
library(data.table)
library(zoo)



## Class for ODKForm
setClass('XLSForm', slots=list(path='character', survey='data.table',
                               repeats='data.table', choices='data.table',
                               combine_views='data.table'))

######################
### Set Generic methods for class
setGeneric('processForm', function(.Object, ...){
  standardGeneric('processForm')
})
setGeneric('calculateTypes', function(.Object, ...){
  standardGeneric('calculateTypes')
})
setGeneric('addInstanceID', function(.Object, ...){
  standardGeneric('addInstanceID')
})
setGeneric('configureViews', function(.Object, ...){
  standardGeneric('configureViews')
})
setGeneric('genSQLBase', function(.Object, ...){
  standardGeneric('genSQLBase')
})
setGeneric('genSQLRepeatLevel1', function(.Object, ...){
  standardGeneric('genSQLRepeatLevel1')
})
setGeneric('genSQLRepeatLevel1_combine', function(.Object, ...){
  standardGeneric('genSQLRepeatLevel1_combine')
})
setGeneric('genSQLRepeatLevel2', function(.Object, ...){
  standardGeneric('genSQLRepeatLevel2')
})
setGeneric('subsetView', function(.Object, ...){
  standardGeneric('subsetView')
})
setGeneric('genSQL', function(.Object, ...){
  standardGeneric('genSQL')
})
setGeneric('genSQLAllRepeats', function(.Object, ...){
  standardGeneric('genSQLAllRepeats')
})
setGeneric('genFullViewNames', function(.Object, ...){
  standardGeneric('genFullViewNames')
})
setGeneric('genPermissionsSQL', function(.Object, ...){
  standardGeneric('genPermissionsSQL')
})


########################################
### Set of functions to initialize
##  as part of initialization we want to process the form and include instanceID

# append the instanceID field to the form
setMethod('addInstanceID', 'XLSForm', function(.Object){
  # add instanceid into the form
  .Object@survey=rbind(.Object@survey,
                       list(type='text', name='instanceID',
                            repeat_indicator=0, group_indicator=0,
                            node_name=list(c('data', 'meta', 'instanceID')),
                            repeat_depth=0,
                            repeat_parent=NA))
  return(.Object)
})


setMethod('processForm', 'XLSForm',
          function(.Object){
  .Object@survey[,`:=` ('type'=str_trim(type),
                        'name'=str_trim(name))]
  
  
  # remove notes and end
  .Object@survey=.Object@survey[!type %in% c('note', 'end')]
  
  #### create path to object
  # first create columns to know when repeat or group is starting/ending
  .Object@survey[, repeat_indicator:=fcase(grepl('begin repeat', type), 1,
                                           grepl('end repeat', type), -1,
                                           default=0)]
  .Object@survey[, group_indicator:=fcase(grepl('begin group', type), 1,
                                          grepl('end group', type), -1,
                                          default=0)]
  
  # Create a column with a list that has the path to the node
  node_name=vector(mode='character', nrow(.Object@survey))
  thisnode='data'
  for(i in 1:nrow(.Object@survey)){
    if(.Object@survey$repeat_indicator[i]==1|.Object@survey$group_indicator[i]==1){
      thisnode=c(thisnode, .Object@survey$name[i])
      node_name[i]=list(thisnode)
    } else if(.Object@survey$repeat_indicator[i]==-1|.Object@survey$group_indicator[i]==-1){
      thisnode=head(thisnode, -1)
      node_name[i]=list(thisnode)
    } else {
      node_name[i]=list(c(thisnode, .Object@survey$name[i]))
    }
  }
  .Object@survey$node_name=node_name
  
  # Now create a repeat depth column (each cascading repeat needs another view)
  .Object@survey[, repeat_depth:=cumsum(repeat_indicator)]
  # And another with the name of the parent repeat (if repeat inside a repeat)
  .Object@survey[repeat_depth>0,
                 repeat_parent:=na.locf(fifelse(repeat_indicator==1, name, as.character(NA)))]
  .Object@survey[repeat_depth==1, # fix nodes after exiting level-2 repeat
                 repeat_parent:=na.locf(fifelse(repeat_indicator==1, name, as.character(NA)))]
  .Object@survey[repeat_indicator==1, # fix parent for begin repeats
                 repeat_parent:=unlist(node_name[[1]][length(node_name[[1]])-1]), by=name]
  .Object@survey[repeat_indicator==1 & repeat_depth==1, # set parent to NA for 1st level repeats
                 repeat_parent:=NA]
  .Object <- addInstanceID(.Object)
  
  # set the repeats attribute
  .Object@repeats <- .Object@survey[repeat_indicator==1 | group_indicator==1]
  # remove begin/end repeats and groups from the survey object
  .Object@survey <- .Object@survey[group_indicator==0 & repeat_indicator==0]
  
  
  ########
  # add various paths to use in sql queries
  ## direct path (use for anything outside repeat)
  .Object@survey[,path:=paste0(' as_json -> \'',
                               paste(head(unlist(node_name), -1),
                                     collapse='\' -> \''),
                               '\' ->> \'',
                               tail(unlist(node_name), 1),
                               '\''),
                 by=name]
  ## path for repeat -- use with jsonb_to_recordset()
  .Object@repeats[,path_repeat:=paste0(' as_json -> \'',
                                       paste(head(unlist(node_name), -1),
                                             collapse='\' -> \''),
                                       '\' -> \'',
                                       tail(unlist(node_name), 1),
                                       '\''),
                  by=name]
  
  ## column for the direct parent
  .Object@survey[,direct_parent:=unlist(node_name[[1]][length(node_name[[1]])-1]), by=name]
  
  ## relative path from repeat_parent
  .Object@survey[,repeat_relative_path:=sapply(strsplit(path, paste0(repeat_parent, '\'')), '[', 2),
                 by=name]
  ## relative path from repeat_parent as json (for 2nd level repeats)
  .Object@repeats[,repeat_relative_path_asjson:=sapply(strsplit(path_repeat, paste0(repeat_parent, '\'')), '[', 2),
                 by=name]
  
  
  # change single to double quotes and remove leading '->' for use with laterals
  path_splits=lapply(gsub("^[^\']*", "", .Object@survey$repeat_relative_path),
                     strsplit, split='->')
  new_paths=sapply(path_splits, function(s){
    ifelse(length(s[[1]])>1,
           paste(c(gsub('\'', '\"', s[[1]][1]),
                   s[[1]][2:length(s[[1]])]),
                 collapse='->'),
           gsub('\'', '\"', s[[1]])
    )
  })
  .Object@survey[,lateral_path:=str_trim(new_paths)]
  
  .Object@survey[,newname:=tolower(name)]
  return(.Object)
})

setMethod('configureViews', 'XLSForm',
          function(.Object, include_cols=NULL, change_names=NULL){
            # verify combine_views
            if(nrow(.Object@combine_views)>0){
              needcols=c('repeat_name', 'view', 'name_old', 'name_new', 'repeat_id')
              if(!all(needcols %in% colnames(.Object@combine_views))){
                missingcol=which(!needcols %in% colnames(.Object@combine_views))
                stop(paste('combine_views does not contain column(s):', paste(missingcol, collapse=', ')))
              }
              name_mismatch=.Object@combine_views[!name_old %in% .Object@survey[,name], unique(name_old)]
              if(length(name_mismatch)>0){
                stop(paste('Mismatching column names found in combine_views:\n',
                           paste(name_mismatch, collapse=', ')))
              }
            }
            if(!is.null(include_cols)){
              name_mismatch_include=names(include_cols)[which(!names(include_cols) %in% .Object@survey[,name])]
              if(length(name_mismatch)>0){
                stop(paste('Mismatching column names found in include_cols:\n',
                           paste(name_mismatch_include, collapse=', ')))
              }
            }
            
            # merge in any nodes in a repeat in combine_views but not there yet
            if(nrow(.Object@combine_views)>0){
              .Object@combine_views <- rbindlist(list(
                .Object@survey[!name %in% .Object@combine_views[,name_old] &
                                 repeat_parent %in% .Object@combine_views[,unique(repeat_name)],
                               .(repeat_name_col=.Object@combine_views[repeat_name %in% repeat_parent,
                                                                       unique(repeat_name_col)][1],
                                 repeat_name=repeat_parent,
                                 view=fifelse(repeat_parent %in% .Object@combine_views[,unique(view)],
                                              repeat_parent,
                                              .Object@combine_views[repeat_name %in% repeat_parent, unique(view)][1]),
                                 name_old=name,
                                 name_new=tolower(name),
                                 repeat_id=.Object@combine_views[repeat_name %in% repeat_parent,
                                                                 unique(repeat_id)]),
                               by=repeat_parent][,!"repeat_parent"],
                .Object@combine_views))
            }
            
            ## PART I: VIEWS (what view should a node in a repeat be part of)
            if(nrow(.Object@combine_views)>0){
              .Object@survey[.Object@combine_views, on='name==name_old',
                             repeat_view:=i.view]
            } else {
              .Object@survey[,repeat_view:=as.character(NA)]
            }
            
            .Object@survey[is.na(repeat_view), repeat_view:=repeat_parent]
            
            ## PART II: create columns to specify which nodes to include in each view
            .Object@survey[,include_base:=(repeat_depth==0)]
            
            # now do the repeat views
            # for include_cols: value should be always or the name of a view
            repeat_views= .Object@survey[repeat_depth>0,unique(repeat_view)]
            for(v in repeat_views){
              set( .Object@survey, j=paste0('include_', v),
                   value= .Object@survey[,(grepl(v,repeat_view)|
                                             (name %in% names(include_cols) &
                                                include_cols[name] %in% c('always', v)))])
            }
            

            # fix the columns that are in combine_views
            if(nrow(.Object@combine_views)>0){
              .Object@survey[.Object@combine_views, on='name==name_old',
                             newname:=i.name_new]
            }
            # renaming newname for the query as specified
            .Object@survey[name %in% names(change_names),
                           newname:=change_names[name]]
            return(.Object)
          })



# Main initializer for the class
setMethod('initialize', 'XLSForm',
          function(.Object, path,
                   combine_views=data.table(), include_cols=NULL, change_names=NULL,
                   exclude_cols=NULL){
  .Object@path <- path
  .Object@survey <- data.table(read.xlsx(path, sheet='survey'))[,.(type, name)]
  if(!is.null(exclude_cols)){
    .Object@survey <- .Object@survey[!name %in% exclude_cols]
  }
  .Object@choices <- data.table(read.xlsx(path, sheet='choices'))[,.(list_name, name)]
  .Object@combine_views <- combine_views
  .Object <- processForm(.Object)
  .Object <- configureViews(.Object, include_cols=include_cols, change_names=change_names)
  return(.Object)
})


# calculate the correct type for postgres to use
setMethod('calculateTypes', 'XLSForm', 
          function(.Object, calc_types=NULL, calc_default='text', type_default='text'){
            # parse the choices to return select_ones where we can't use integer
            .Object@choices[,name.int:=as.integer(name)]
            choiceslists=.Object@choices[!is.na(list_name),.(coercable=.SD[is.na(name.int),.N]==0),by=list_name]
            choiceslists[grepl('customize_me', list_name), coercable:=F]
            choices_usetext=choiceslists[coercable==F,list_name]
            
            # define types to use for postgres
            .Object@survey[,postgres_type:=fcase(grepl('select_multiple', type), 'text',
                                                 grepl(paste(choices_usetext, collapse='|'), type), 'text',
                                                 grepl('integer|select_one', type), 'int',
                                                 grepl('float', type), 'numeric',
                                                 grepl('calculate', type), ifelse(name %in% names(calc_types),
                                                                                  calc_types[name],
                                                                                  calc_default),
                                                 default=type_default)]
            
          })

setMethod('subsetView', 'XLSForm', function(.Object, repeat_view_name){
  .Object@survey <- .Object@survey[get(paste0('include_', repeat_view_name))==T]
  return(.Object)
})







#####################################################################
###################### GENERATING SQL QUERIES #######################

setMethod('genSQLBase', 'XLSForm', 
          function(.Object, schema, view_name, form_id, startdate, enddate,
                   view_prefix='', form_version=NA, filter_version_sql=F,
                   source_table='openhds.form_submission',
                   distinct_on=NULL){
            if(filter_version_sql==T & is.na(form_version)){
              stop("To filter by form version form_version must be specified")
            }
            # trim trailing _ from view name
            if(substr(view_name, nchar(view_name), nchar(view_name)) %in% c('_', '-')){
              view_name=substr(view_name, 1, nchar(view_name)-1)
            }
            # parse the distinct on
            distinct_on_sql=paste0('DISTINCT ON (',
                                   paste(.Object@survey[name %in% distinct_on, path],
                                         collapse=',\n'),
                                   ')\n')
            # generate the sql
            paste0('CREATE OR REPLACE VIEW \"', schema, '\".', view_prefix, view_name,
                   ' AS \nSELECT ',
                   ifelse(!is.null(distinct_on),
                          distinct_on_sql,
                          ''),
                   paste(paste0(ifelse(.Object@survey[include_base==T,postgres_type %in% c('numeric', 'int')],
                                       paste0('(CASE WHEN ',
                                              .Object@survey[include_base==T, path],
                                              '=\'\' THEN NULL ELSE '),
                                       '('),
                                .Object@survey[include_base==T, path],
                                ifelse(.Object@survey[include_base==T,postgres_type %in% c('numeric', 'int')],
                                       ' END ',
                                       ''),
                                ')::', .Object@survey[include_base==T,postgres_type],
                                ' as ',
                                .Object@survey[include_base==T,newname]),
                         collapse=',\n'), '
                    FROM ', source_table,
                   '\nWHERE form_id=\'', form_id, '\'',
                   ifelse(filter_version_sql==T,
                          paste0(' AND form_version=\'', form_version, '\''),
                          ''),'
                    AND collected::date >= \'', format(startdate, '%Y-%m-%d'),'\'
                    AND collected::date < \'', format(enddate, '%Y-%m-%d'), '\';')
          })


# query for 1st level repeat without combining any repeats
setMethod('genSQLRepeatLevel1', 'XLSForm',
          function(.Object, schema, view_name, form_id, startdate, enddate,
                   view_prefix='', form_version=NA, filter_version_sql=F,
                   source_table='openhds.form_submission',
                   distinct_on=NULL){
            # parse the distinct on
            distinct_on_sql=paste0('DISTINCT ON (',
                                   paste(.Object@survey[name %in% distinct_on, path],
                                         collapse=',\n'),
                                   ')\n')
            # subset to the relevant view
            .Object=subsetView(.Object, view_name)
            # generate the sql
            paste0('CREATE OR REPLACE VIEW \"', schema, '\".', view_prefix, view_name,
                   ' AS ',
                   '\nSELECT ',
                   ifelse(!is.null(distinct_on),
                          distinct_on_sql,
                          ''),
                   paste(paste('(', .Object@survey[repeat_depth==0, path],
                               ')::', .Object@survey[repeat_depth==0, postgres_type],
                               'as',
                               .Object@survey[repeat_depth==0, newname]),
                         collapse=',\n'), ',\n',
                   paste(paste0(ifelse(.Object@survey[repeat_depth==1,postgres_type %in% c('numeric', 'int')],
                                       paste0('(CASE WHEN r.',
                                              .Object@survey[repeat_depth==1,lateral_path],
                                              '=\'\' THEN NULL ELSE '),
                                       '('),
                                'r.',
                                .Object@survey[repeat_depth==1,lateral_path],
                                ifelse(.Object@survey[repeat_depth==1,postgres_type %in% c('numeric', 'int')],
                                       ' END ',
                                       ''),
                                ')::', .Object@survey[repeat_depth==1,postgres_type],
                                ' as ',
                                .Object@survey[repeat_depth==1,newname]),
                         collapse=',\n'),
                   '\nFROM openhds.form_submission,',
                   '\nLATERAL jsonb_to_recordset(',
                   .Object@repeats[name==.Object@survey[!is.na(repeat_parent),unique(repeat_parent,)],
                                   path_repeat],
                   ')
                          r(',
                   paste(
                     paste(
                       paste0('\"',
                              c(.Object@survey[repeat_depth==1 &
                                                 repeat_parent==direct_parent,
                                               name],
                                .Object@survey[repeat_depth==1 &
                                                 repeat_parent!=direct_parent,
                                               unique(direct_parent)]),
                              '\"'),
                       paste(c(.Object@survey[repeat_depth==1 &
                                                repeat_parent==direct_parent,
                                              # set numeric to text to avoid conversion errors
                                              # (coerce above in the query instead)
                                              ifelse(postgres_type %in% c('numeric', 'int'), 'text', postgres_type)],
                               .Object@survey[repeat_depth==1 &
                                                repeat_parent!=direct_parent,
                                              rep('jsonb', length(unique(direct_parent)))]
                       ))
                     ),
                     collapse=', '),
                   ')
                          WHERE form_id=\'', form_id, '\'',
                   ifelse(filter_version_sql==T,
                          paste0(' AND form_version=\'', form_version, '\''),
                          ''),'
                          AND collected::date >= \'', format(startdate, '%Y-%m-%d'),'\'
                         AND collected::date < \'', format(enddate, '%Y-%m-%d'), '\';')
          })



# generate query for a combination of 1st level repeats
setMethod('genSQLRepeatLevel1_combine', 'XLSForm',
          function(.Object, schema, view_name, form_id, startdate, enddate,
                   view_prefix='', form_version=NA, filter_version_sql=F,
                   source_table='openhds.form_submission',
                   distinct_on=NULL){
            # parse the distinct on
            distinct_on_sql=paste0('DISTINCT ON (',
                                  paste(.Object@survey[name %in% distinct_on, path],
                                        collapse=',\n'),
                                  ')\n')
            # subset to the relevant view
            .Object=subsetView(.Object, view_name)
            # what are the repeats we are combining
            repeats=.Object@survey[!is.na(repeat_parent),unique(repeat_parent)]
            view_cols=.Object@survey[repeat_depth>0,unique(newname)]
            # generate the sql
            paste0('CREATE OR REPLACE VIEW \"', schema, '\".', view_prefix, view_name,' AS\n', 
                   paste(sapply(repeats, function(rpt){
                     paste0('SELECT ',
                            ifelse(!is.null(distinct_on),
                                   distinct_on_sql,
                                   ''),
                            # things outside the repeat
                            paste(paste('(', .Object@survey[repeat_depth==0, path],
                                        ')::', .Object@survey[repeat_depth==0, postgres_type],
                                        'as',
                                        .Object@survey[repeat_depth==0, newname]),
                                  collapse=',\n'), ',\n',
                            # repeat_name
                            paste0('\'', .Object@combine_views[repeat_name==rpt,unique(repeat_id)],
                                   '\' as ', .Object@combine_views[repeat_name==rpt,unique(repeat_name_col)],',\n'),
                            # inside the repeat
                            paste(
                              sapply(view_cols, function(vc){
                                node=.Object@survey[repeat_parent==rpt & newname==vc]
                                if(nrow(node)>0){
                                  column_def=paste0(fifelse(node[,postgres_type %in% c('numeric', 'int')],
                                                            paste0('(CASE WHEN l_', rpt, '.',
                                                                   node[,lateral_path],
                                                                   '=\'\' THEN NULL ELSE ',
                                                                   'l_', rpt, '.', node[,lateral_path],
                                                                   ' END'),
                                                            paste0('(l_', rpt, '.', node[,lateral_path])),
                                                    ')::',
                                                    node[,postgres_type],
                                                    ' as ', vc)
                                } else{
                                  column_def=paste0('(NULL) as ', vc)
                                } 
                                column_def
                              }),
                              collapse=',\n'),
                            '\nFROM openhds.form_submission,\n',
                            'LATERAL jsonb_to_recordset(',
                            .Object@repeats[name == rpt, path_repeat],
                            ') l_', rpt, '(',
                            paste(paste0(
                              '\"',
                              c(.Object@survey[direct_parent==rpt, name],
                                .Object@survey[repeat_depth == 1 &
                                                 repeat_parent!=direct_parent &
                                                 repeat_parent==rpt, 
                                               unique(direct_parent)]),
                              '\" ',
                              c(rep('text', .Object@survey[direct_parent==rpt, .N]),
                                rep('jsonb', .Object@survey[repeat_depth == 1 &
                                                              repeat_parent!=direct_parent &
                                                              repeat_parent==rpt, length(unique(direct_parent))]))
                            ),
                            collapse = ' ,'),
                            ')\n',
                            'WHERE form_id=\'', form_id, '\'',
                            ifelse(filter_version_sql==T,
                                   paste0(' AND form_version=\'', form_version, '\''),
                                   ''),'
                            AND collected::date >= \'', format(startdate, '%Y-%m-%d'),'\'
                           AND collected::date < \'', format(enddate, '%Y-%m-%d'), '\'')
                   }),
                   collapse='\n\nUNION ALL\n\n'),
                   ';')
          })


setMethod('genSQLRepeatLevel2', 'XLSForm',
          function(.Object, schema, view_name, form_id, startdate, enddate,
                   view_prefix='', form_version=NA, filter_version_sql=F,
                   source_table='openhds.form_submission',
                   distinct_on=NULL){
            parent=.Object@repeats[name %in% .Object@survey[repeat_depth==2,repeat_parent],
                        unique(repeat_parent)]
            if(length(parent)>1){
              stop(paste0('Could not create view ',
                          'mis_', mis_year, '_', v, '\n',
                          'Two second-level repeats with different repeat parents cannot be combined'))
            }
            if(nrow(.Object@combine_views)>0){
              view_cols=.Object@combine_views[view==view_name,unique(name_new)]
            } else{
              view_cols=NULL
            }
            # parse the distinct on
            distinct_on_sql=paste0('DISTINCT ON (',
                                   paste(.Object@survey[name %in% distinct_on, path],
                                         collapse=',\n'),
                                   ')\n')
            # subset to the relevant view
            .Object=subsetView(.Object, view_name)
            # generate the sql (subquery with parent repeat elements)
            subquery=paste0('WITH repeat AS (SELECT ',
                            ifelse(!is.null(distinct_on),
                                   distinct_on_sql,
                                   ''),
                            paste(paste('(', .Object@survey[repeat_depth==0, path],
                                        ')::', .Object@survey[repeat_depth==0, postgres_type],
                                        'as',
                                        .Object@survey[repeat_depth==0, newname]),
                                  collapse=',\n'), ',\n',
                            'jsonb_array_elements(',
                            .Object@repeats[name==parent,path_repeat],
                            ') as element
                                     FROM ', source_table,
                                     '\nWHERE form_id=\'', form_id, '\'',
                            ifelse(filter_version_sql==T,
                                   paste0(' AND form_version=\'', form_version, '\''),
                                   ''),'
                                     AND collected::date >= \'', format(startdate, '%Y-%m-%d'),'\'
                                     AND collected::date < \'', format(enddate, '%Y-%m-%d'), '\'',
                            ')\n')
            mainquery=paste(sapply(.Object@survey[repeat_depth==2,unique(repeat_parent)],
                                   function(rpt){
                                     if(is.null(view_cols)){ # we aren't combining with other views
                                       cols_to_pull=paste(
                                         # nodes in the parent repeat
                                         c(paste('element',
                                                 .Object@survey[repeat_depth==1,repeat_relative_path],
                                                 'as',
                                                 .Object@survey[repeat_depth==1,newname]),
                                           # nodes in the level-2 repeat
                                           paste0('(l_', rpt, '.',
                                                  .Object@survey[repeat_parent==rpt,lateral_path],
                                                  ')::', .Object@survey[repeat_parent==rpt,postgres_type],
                                                  ' as ',
                                                  .Object@survey[repeat_parent==rpt,newname])),
                                         collapse=',\n')
                                     } else {
                                       cols_to_pull=paste(
                                         # nodes in the parent repeat
                                         c(paste0('(element',
                                                  .Object@survey[repeat_depth==1,repeat_relative_path],
                                                  ')::', .Object@survey[repeat_depth==1,postgres_type],
                                                  ' as ',
                                                  .Object@survey[repeat_depth==1,newname]),
                                           # name of repeat (we are unioning different repeats)
                                           paste0('\'', .Object@combine_views[view==view_name & repeat_name==rpt,
                                                                              unique(repeat_id)][1],
                                                  '\' as ', .Object@combine_views[view==view_name & repeat_name==rpt,
                                                                                  unique(repeat_name_col)][1]),
                                           # nodes in the level-2 repeat
                                           sapply(view_cols, function(c){
                                             thispath=.Object@survey[repeat_parent==rpt & newname==c,
                                                                lateral_path]
                                             paste0(ifelse(length(thispath)>0,
                                                           paste0('(', 'l_', rpt, '.',
                                                                  thispath,
                                                                  ')::',
                                                                  .Object@survey[repeat_parent==rpt & newname==c,
                                                                            postgres_type]),
                                                           'NULL'),
                                                    ' as ', c)
                                           })),
                                         collapse=',\n')
                                     }
                                     paste0('SELECT ',
                                            paste(.Object@survey[repeat_depth==0, newname],
                                                  collapse=',\n'), ',\n',
                                            cols_to_pull,
                                            '\nFROM repeat,',
                                            '\nLATERAL jsonb_to_recordset(element',
                                            .Object@repeats[name==rpt,repeat_relative_path_asjson],
                                            ')\n',
                                            'l_', rpt, '(\"',
                                            paste0(
                                              paste0(.Object@survey[repeat_parent==rpt &
                                                                 repeat_depth==2,
                                                               ifelse(repeat_parent==direct_parent,
                                                                      name,
                                                                      direct_parent)],
                                                     '\" text'),
                                              collapse=', \"'),
                                            ')\n'
                                     )
                                   }))
            # put the view creation command together
            paste0('CREATE OR REPLACE VIEW \"', schema, '\".', view_prefix, view_name,
                   ' AS\n',
                   subquery,
                   paste(mainquery, collapse='\nUNION ALL\n\n'))
          })



setMethod('genSQL', 'XLSForm', function(.Object, schema, view_name, form_id, startdate, enddate,
                                        base=F, view_prefix='', form_version=NA,
                                        filter_version_sql=F,
                                        source_table='openhds.form_submission',
                                        distinct_on=NULL){
  # add _ after prefix if missing
  if(!substr(view_prefix, nchar(view_prefix), nchar(view_prefix)) %in% c('_', '-') &
        nchar(view_prefix)>1){
    view_prefix=paste0(view_prefix, '_')
  }
  if(base==F){
    if(!view_name %in% .Object@survey[,unique(repeat_view)]){
      stop(paste0("No repeat with name \'", view_name, "\'\nDid you mean to set base=TRUE?"))
    }
    subset_view=subsetView(.Object, view_name)
    maxlev=subset_view@survey[,max(repeat_depth)]
    combined=view_name %in% subset_view@combine_views[,unique(view)]
    if(maxlev==1 & combined==F){
      sql=genSQLRepeatLevel1(.Object, schema=schema, view_name=view_name, form_id=form_id,
                             startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                             form_version=form_version, filter_version_sql=filter_version_sql,
                             source_table=source_table, distinct_on=distinct_on)
    } else if(maxlev==1 & combined==T){
      sql=genSQLRepeatLevel1_combine(.Object, schema=schema, view_name=view_name, form_id=form_id,
                                     startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                                     form_version=form_version, filter_version_sql=filter_version_sql,
                                     source_table=source_table, distinct_on=distinct_on)
    } else if (maxlev==2) {
      sql=genSQLRepeatLevel2(.Object, schema=schema, view_name=view_name, form_id=form_id,
                             startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                             form_version=form_version, filter_version_sql=filter_version_sql,
                             source_table=source_table, distinct_on=distinct_on)
    }
    
  } else{
    if(view_name %in% .Object@survey[,unique(repeat_view)]){
      stop(paste0("View name \'", view_name, "\' is a view for a repeat\nDid you mean to set base=FALSE?"))
    }
    sql=genSQLBase(.Object, schema=schema, view_name=view_name, form_id=form_id,
                   startdate=startdate, enddate=enddate, view_prefix=view_prefix,
                   form_version=form_version, filter_version_sql=filter_version_sql,
                   source_table=source_table, distinct_on=distinct_on)
  }
})

setMethod('genSQLAllRepeats', 'XLSForm', function(.Object, schema, view_name, form_id, startdate, enddate,
                                        base=F, view_prefix='', form_version=NA,
                                        filter_version_sql=F,
                                        source_table='openhds.form_submission',
                                        distinct_on=NULL){
  repeats=.Object@survey[repeat_depth>0,unique(repeat_view)]
  if(NA %in% repeats){
    stop('Not all fields in repeat have a defined repeat_view')
  }
  sql=sapply(repeats, function(rpt){
    genSQL(.Object, schema=schema, view_name=rpt, form_id=form_id,
           startdate=startdate, enddate=enddate, view_prefix=view_prefix,
           form_version=form_version, filter_version_sql=filter_version_sql,
           source_table=source_table, distinct_on=distinct_on)
  })
})

setMethod('genFullViewNames', 'XLSForm', function(.Object, schema, base_name, view_prefix=''){
  paste0('\"', schema, '\".', view_prefix,
         ifelse(!substr(view_prefix, nchar(view_prefix), nchar(view_prefix)) %in% c('_', '-') &
                  nchar(view_prefix)>1,
                '_', ''),
         c(base_name, .Object@survey[!is.na(repeat_view),unique(repeat_view)]))
})

setMethod('genPermissionsSQL', 'XLSForm', function(.Object, schema, base_name, owner, view_prefix=''){
  views=genFullViewNames(.Object, schema=schema, base_name=base_name, view_prefix=view_prefix)
  sql=paste('ALTER TABLE', views, 'OWNER TO', owner)
})