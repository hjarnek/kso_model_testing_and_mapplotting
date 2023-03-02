# -*- coding: utf-8 -*-

from qgis.PyQt.QtCore import QCoreApplication, QVariant
from qgis.core import (QgsProcessing,
                       QgsProcessingException,
                       QgsProcessingAlgorithm,
                       QgsProcessingParameterFeatureSource,
                       QgsProcessingParameterFeatureSink,
                       QgsProcessingParameterField,
                       QgsProcessingParameterNumber,
                       QgsField,
                       QgsExpression,
                       QgsExpressionContext,
                       QgsExpressionContextUtils,
                       edit,
                       QgsEditError,
                       QgsProperty)
from qgis import processing


class LineSegmentTimes(QgsProcessingAlgorithm):

    # Constants used to refer to parameters and outputs.
    INPUT = 'INPUT'
    TIMESTAMPS = 'TIMESTAMPS'
    SECONDS = 'SECONDS'
    OUTPUT = 'OUTPUT'

    def tr(self, string):
        """
        Returns a translatable string with the self.tr() function.
        """
        return QCoreApplication.translate('Processing', string)

    def createInstance(self):
        return LineSegmentTimes()

    def name(self):
        """
        Returns the algorithm name, used for identifying the algorithm. This
        string should be fixed for the algorithm, and must not be localised.
        The name should be unique within each provider. Names should contain
        lowercase alphanumeric characters only and no spaces or other
        formatting characters.
        """
        return 'linesegmenttimes'

    def displayName(self):
        """
        Returns the translated algorithm name, which should be used for any
        user-visible display of the algorithm name.
        """
        return self.tr('Line segment times')

    def group(self):
        """
        Returns the name of the group this algorithm belongs to. This string
        should be localised.
        """
        return self.tr('Route processing')

    def groupId(self):
        """
        Returns the unique ID of the group this algorithm belongs to. This
        string should be fixed for the algorithm, and must not be localised.
        The group id should be unique within each provider. Group id should
        contain lowercase alphanumeric characters only and no spaces or other
        formatting characters.
        """
        return 'routeprocessing'

    def shortHelpString(self):
        """
        Returns a localised short helper string for the algorithm. This string
        should provide a basic description about what the algorithm does and the
        parameters and outputs associated with it.
        """
        return self.tr("Take an input layer of waypoints and create a timed route polyline layer where the length of each line segment corresponds to a given number of seconds.")

    def helpUrl(self):
        return "https://github.com/hjarnek/kso_model_testing_and_mapplotting/tree/main/geoprocessing"

    def AddField(self, layer, field, fieldtype, expr):
        expression = QgsExpression(expr)
        context = QgsExpressionContext()
        context.appendScopes(QgsExpressionContextUtils.globalProjectLayerScopes(layer))
        try:
            with edit(layer):
                layer.dataProvider().addAttributes([QgsField(field, fieldtype)])
                layer.updateFields()
                for f in layer.getFeatures():
                    context.setFeature(f)
                    f[field] = expression.evaluate(context)
                    layer.updateFeature(f)
        except QgsEditError as err:
            print(repr(err))


    def initAlgorithm(self, config=None):
        """
        Here we define the inputs and output of the algorithm, along
        with some other properties.
        """
        
        self.addParameter(
            QgsProcessingParameterFeatureSource(
                self.INPUT,
                self.tr('Input point layer'),
                types=[QgsProcessing.TypeVectorPoint]
            )
        )
        
        self.addParameter(
            QgsProcessingParameterField(
                self.TIMESTAMPS,
                self.tr('Timestamp field'),
                parentLayerParameterName = self.INPUT,
                defaultValue = 'UNIXTime'
            )
        )
        
        self.addParameter(
            QgsProcessingParameterNumber(
                self.SECONDS,
                self.tr('Seconds corresponding to line segment length'),
                type = QgsProcessingParameterNumber.Double,
                defaultValue = 1,
                minValue = 0.01
            )
        )
        
        self.addParameter(
            QgsProcessingParameterFeatureSink(
                self.OUTPUT,
                self.tr('Output layer')
            )
        )


    def processAlgorithm(self, parameters, context, feedback):
        """
        Here is where the processing itself takes place.
        """
        
        # If any of the required inputs are missing, throw an exception to
        # indicate that the algorithm encountered a fatal error.
        if self.INPUT is None:
            raise QgsProcessingException(self.invalidSourceError(parameters, self.INPUT))
        if self.TIMESTAMPS is None:
            raise QgsProcessingException(self.invalidSourceError(parameters, self.TIMESTAMPS))
        if self.SECONDS is None:
            raise QgsProcessingException(self.invalidSourceError(parameters, self.SECONDS))
        
        # Retrieve the feature source.
        source = self.parameterAsVectorLayer(parameters, self.INPUT, context)
        layername = source.id()
        fieldname = parameters[self.TIMESTAMPS]
        source_first_fid = next(source.getFeatures()).id()
        if (source_first_fid != 0) and (source_first_fid != 1):
            raise QgsProcessingException('ERROR: The first feature ID of the input \
            point layer is neither 0 nor 1. Must be either to continue.')
        
        # Send some information to the user
        feedback.pushInfo('CRS is {}'.format(source.sourceCrs().authid()))
        feedback.pushInfo('Number of points: {}'.format(source.featureCount()))
        
        layer1_line = processing.run("native:pointstopath", {
            'INPUT':parameters[self.INPUT],
            'CLOSE_PATH':False,
            'ORDER_EXPRESSION':fieldname,
            'NATURAL_SORT':False,
            'GROUP_EXPRESSION':'',
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer2_exploded = processing.run("native:explodelines", {
            'INPUT':layer1_line,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer2_exploded_layerobject = context.getMapLayer(layer2_exploded)
        first_fid = next(layer2_exploded_layerobject.getFeatures()).id()
        diff = source_first_fid - first_fid
        expr_enter = 'attributes(get_feature_by_id(\'{}\', ($id+{})))[\'{}\']'\
        .format(layername, diff, fieldname)
        expr_exit = 'attributes(get_feature_by_id(\'{}\', ($id+{}+1)))[\'{}\']'\
        .format(layername, diff, fieldname)
        expr_speed = '$length / ((attributes(get_feature_by_id(\'{}\', ($id+1+{})\
        ))[\'{}\']) - (attributes(get_feature_by_id(\'{}\', ($id+{})))[\'{}\']))'\
        .format(layername, diff, fieldname, layername, diff, fieldname)
        
        self.AddField(layer2_exploded_layerobject, 'id', QVariant.Int, '$id')
        self.AddField(layer2_exploded_layerobject, 'enter', QVariant.Double, expr_enter)
        self.AddField(layer2_exploded_layerobject, 'exit', QVariant.Double, expr_exit)
        self.AddField(layer2_exploded_layerobject, 'speed', QVariant.Double, expr_speed)
        
        f1id = layer2_exploded_layerobject.fields().lookupField('begin')
        f2id = layer2_exploded_layerobject.fields().lookupField('end')
        layer2_exploded_layerobject.dataProvider().deleteAttributes([f1id, f2id])
        layer2_exploded_layerobject.updateFields()
        
        layer3_interpolated = processing.run("native:pointsalonglines", {
            'INPUT':layer2_exploded,
            'DISTANCE':QgsProperty.fromExpression('\"speed\" * {}'.format(parameters[self.SECONDS])),
            'START_OFFSET':0,
            'END_OFFSET':0,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        expr_enter = '\"enter\" + (\"distance\" / \"speed\")'
        expr_exit = 'array_agg(\"enter\")[(@row_number+1)]'
        
        layer4_fieldcalc = processing.run("native:fieldcalculator", {
            'INPUT':layer3_interpolated,
            'FIELD_NAME':'enter',
            'FIELD_TYPE':0,
            'FIELD_LENGTH':0,
            'FIELD_PRECISION':0,
            'FORMULA':expr_enter,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer5_fieldcalc = processing.run("native:fieldcalculator", {
            'INPUT':layer4_fieldcalc,
            'FIELD_NAME':'exit',
            'FIELD_TYPE':0,
            'FIELD_LENGTH':0,
            'FIELD_PRECISION':0,
            'FORMULA':expr_exit,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer6_fieldcalc = processing.run("native:fieldcalculator", {
            'INPUT':layer5_fieldcalc,
            'FIELD_NAME':'id',
            'FIELD_TYPE':1,
            'FIELD_LENGTH':0,
            'FIELD_PRECISION':0,
            'FORMULA':'$id',
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer7_line = processing.run("native:pointstopath", {
            'INPUT':layer6_fieldcalc,
            'CLOSE_PATH':False,
            'ORDER_EXPRESSION':'\"enter\"',
            'NATURAL_SORT':False,
            'GROUP_EXPRESSION':'',
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer8_exploded = processing.run("native:explodelines", {
            'INPUT':layer7_line,
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer9_fieldcalc = processing.run("native:fieldcalculator", {
            'INPUT':layer8_exploded,
            'FIELD_NAME':'id',
            'FIELD_TYPE':1,
            'FIELD_LENGTH':0,
            'FIELD_PRECISION':0,
            'FORMULA':'$id',
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer10_droppedfields = processing.run("native:retainfields", {
            'INPUT':layer9_fieldcalc,
            'FIELDS':['id'],
            'OUTPUT':'TEMPORARY_OUTPUT'
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer11_joined = processing.run("native:joinattributestable", {
            'INPUT':layer10_droppedfields,
            'FIELD':'id',
            'INPUT_2':layer6_fieldcalc,
            'FIELD_2':'id',
            'FIELDS_TO_COPY':['enter','exit','speed','distance','angle'],
            'METHOD':1,
            'DISCARD_NONMATCHING':False,
            'PREFIX':'',
            'OUTPUT':parameters[self.OUTPUT]
        }, is_child_algorithm=True, context=context, feedback=feedback)['OUTPUT']
        
        layer11_joined_layerobject = context.getMapLayer(layer11_joined)
        layer11_joined_layerobject.setName('Interpolated line route')
        
        return {self.OUTPUT: layer11_joined}
